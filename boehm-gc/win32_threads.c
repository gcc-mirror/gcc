#if defined(GC_WIN32_THREADS) 

#include "private/gc_priv.h"
#include <windows.h>

#ifdef CYGWIN32
# include <errno.h>

 /* Cygwin-specific forward decls */
# undef pthread_create 
# undef pthread_sigmask 
# undef pthread_join 
# undef dlopen 

# define DEBUG_CYGWIN_THREADS 0

  GC_bool GC_thr_initialized = FALSE;
  void * GC_start_routine(void * arg);
  void GC_thread_exit_proc(void *arg);

#endif

#ifndef MAX_THREADS
# define MAX_THREADS 64
#endif

struct thread_entry {
  LONG in_use;
  DWORD id;
  HANDLE handle;
  void *stack;		/* The cold end of the stack.   */
			/* 0 ==> entry not valid.	*/
			/* !in_use ==> stack == 0	*/
  CONTEXT context;
  GC_bool suspended;

# ifdef CYGWIN32
    void *status; /* hold exit value until join in case it's a pointer */
    pthread_t pthread_id;
# endif

};

volatile GC_bool GC_please_stop = FALSE;

volatile struct thread_entry thread_table[MAX_THREADS];

void GC_push_thread_structures GC_PROTO((void))
{
    /* Unlike the other threads implementations, the thread table here	*/
    /* contains no pointers to the collectable heap.  Thus we have	*/
    /* no private structures we need to preserve.			*/
# ifdef CYGWIN32
  { int i; /* pthreads may keep a pointer in the thread exit value */
    for (i = 0; i < MAX_THREADS; i++)
      if (thread_table[i].in_use) GC_push_all((ptr_t)&(thread_table[i].status),(ptr_t)(&(thread_table[i].status)+1));
  }
# endif
}

void GC_stop_world()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;

#ifdef CYGWIN32
  if (!GC_thr_initialized) ABORT("GC_stop_world() called before GC_thr_init()");
#endif

  GC_please_stop = TRUE;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack != 0
	&& thread_table[i].id != thread_id) {
#     ifdef MSWINCE
        /* SuspendThread will fail if thread is running kernel code */
	while (SuspendThread(thread_table[i].handle) == (DWORD)-1)
	  Sleep(10);
#     else
	/* Apparently the Windows 95 GetOpenFileName call creates	*/
	/* a thread that does not properly get cleaned up, and		*/
	/* SuspendThread on its descriptor may provoke a crash.		*/
	/* This reduces the probability of that event, though it still	*/
	/* appears there's a race here.					*/
	DWORD exitCode; 
	if (GetExitCodeThread(thread_table[i].handle,&exitCode) &&
            exitCode != STILL_ACTIVE) {
          thread_table[i].stack = 0; /* prevent stack from being pushed */
#         ifndef CYGWIN32
            /* this breaks pthread_join on Cygwin, which is guaranteed to  */
	    /* only see user pthreads 					   */
	    thread_table[i].in_use = FALSE;
	    CloseHandle(thread_table[i].handle);
	    BZERO((void *)(&thread_table[i].context), sizeof(CONTEXT));
#         endif
	  continue;
	}
	if (SuspendThread(thread_table[i].handle) == (DWORD)-1)
	  ABORT("SuspendThread failed");
#     endif
      thread_table[i].suspended = TRUE;
    }
}

void GC_start_world()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack != 0 && thread_table[i].suspended
	&& thread_table[i].id != thread_id) {
      if (ResumeThread(thread_table[i].handle) == (DWORD)-1)
	ABORT("ResumeThread failed");
      thread_table[i].suspended = FALSE;
    }
  GC_please_stop = FALSE;
}

# ifdef _MSC_VER
#   pragma warning(disable:4715)
# endif
ptr_t GC_current_stackbottom()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack && thread_table[i].id == thread_id)
      return thread_table[i].stack;
  ABORT("no thread table entry for current thread");
}
# ifdef _MSC_VER
#   pragma warning(default:4715)
# endif

# ifdef MSWINCE
    /* The VirtualQuery calls below won't work properly on WinCE, but	*/
    /* since each stack is restricted to an aligned 64K region of	*/
    /* virtual memory we can just take the next lowest multiple of 64K.	*/
#   define GC_get_lo_stack_addr(s) \
        ((ptr_t)(((DWORD)(s) - 1) & 0xFFFF0000))
# else
    static ptr_t GC_get_lo_stack_addr(ptr_t s)
    {
	ptr_t bottom;
	MEMORY_BASIC_INFORMATION info;
	VirtualQuery(s, &info, sizeof(info));
	do {
	    bottom = info.BaseAddress;
	    VirtualQuery(bottom - 1, &info, sizeof(info));
	} while ((info.Protect & PAGE_READWRITE)
		 && !(info.Protect & PAGE_GUARD));
	return(bottom);
    }
# endif

void GC_push_all_stacks()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack) {
      ptr_t bottom = GC_get_lo_stack_addr(thread_table[i].stack);
      if (thread_table[i].id == thread_id)
	GC_push_all_stack((ptr_t)&i, thread_table[i].stack);
      else {
	thread_table[i].context.ContextFlags
			= (CONTEXT_INTEGER|CONTEXT_CONTROL);
	if (!GetThreadContext(thread_table[i].handle,
				/* cast away volatile qualifier */
	    		        (LPCONTEXT)&thread_table[i].context))
	  ABORT("GetThreadContext failed");
#	ifdef I386
	  GC_push_one ((word) thread_table[i].context.Edi);
    	  GC_push_one ((word) thread_table[i].context.Esi);
    	  GC_push_one ((word) thread_table[i].context.Ebp);
    	  GC_push_one ((word) thread_table[i].context.Ebx);
    	  GC_push_one ((word) thread_table[i].context.Edx);
    	  GC_push_one ((word) thread_table[i].context.Ecx);
    	  GC_push_one ((word) thread_table[i].context.Eax);
	  if (thread_table[i].context.Esp >= (DWORD)thread_table[i].stack
	      || thread_table[i].context.Esp < (DWORD)bottom) {
	      WARN("Thread stack pointer 0x%lx out of range, pushing everything",
		   thread_table[i].context.Esp);
	      GC_push_all_stack((char *) bottom, thread_table[i].stack);
	  } else {
	      GC_push_all_stack((char *) thread_table[i].context.Esp,
			        thread_table[i].stack);
	  }
#       else
#       ifdef ARM32
	  if (thread_table[i].context.Sp >= (DWORD)thread_table[i].stack
	      || thread_table[i].context.Sp < (DWORD)bottom)
	      ABORT("Thread stack pointer out of range");
	  GC_push_one ((word) thread_table[i].context.R0);
	  GC_push_one ((word) thread_table[i].context.R1);
	  GC_push_one ((word) thread_table[i].context.R2);
	  GC_push_one ((word) thread_table[i].context.R3);
	  GC_push_one ((word) thread_table[i].context.R4);
	  GC_push_one ((word) thread_table[i].context.R5);
	  GC_push_one ((word) thread_table[i].context.R6);
	  GC_push_one ((word) thread_table[i].context.R7);
	  GC_push_one ((word) thread_table[i].context.R8);
	  GC_push_one ((word) thread_table[i].context.R9);
	  GC_push_one ((word) thread_table[i].context.R10);
	  GC_push_one ((word) thread_table[i].context.R11);
	  GC_push_one ((word) thread_table[i].context.R12);
	  GC_push_all_stack((char *) thread_table[i].context.Sp,
			    thread_table[i].stack);
#       else
#	ifdef SHx
	  if (thread_table[i].context.R15 >= (DWORD)thread_table[i].stack
	      || thread_table[i].context.R15 < (DWORD)bottom)
	      ABORT("Thread stack pointer out of range");
	  GC_push_one ((word) thread_table[i].context.R0);
	  GC_push_one ((word) thread_table[i].context.R1);
	  GC_push_one ((word) thread_table[i].context.R2);
	  GC_push_one ((word) thread_table[i].context.R3);
	  GC_push_one ((word) thread_table[i].context.R4);
	  GC_push_one ((word) thread_table[i].context.R5);
	  GC_push_one ((word) thread_table[i].context.R6);
	  GC_push_one ((word) thread_table[i].context.R7);
	  GC_push_one ((word) thread_table[i].context.R8);
	  GC_push_one ((word) thread_table[i].context.R9);
	  GC_push_one ((word) thread_table[i].context.R10);
	  GC_push_one ((word) thread_table[i].context.R11);
	  GC_push_one ((word) thread_table[i].context.R12);
	  GC_push_one ((word) thread_table[i].context.R13);
	  GC_push_one ((word) thread_table[i].context.R14);
	  GC_push_all_stack((char *) thread_table[i].context.R15,
			    thread_table[i].stack);
#       else
#	ifdef MIPS
	  if (thread_table[i].context.IntSp >= (DWORD)thread_table[i].stack
	      || thread_table[i].context.IntSp < (DWORD)bottom)
	      ABORT("Thread stack pointer out of range");
	  GC_push_one ((word) thread_table[i].context.IntAt);
	  GC_push_one ((word) thread_table[i].context.IntV0);
	  GC_push_one ((word) thread_table[i].context.IntV1);
	  GC_push_one ((word) thread_table[i].context.IntA0);
	  GC_push_one ((word) thread_table[i].context.IntA1);
	  GC_push_one ((word) thread_table[i].context.IntA2);
	  GC_push_one ((word) thread_table[i].context.IntA3);
	  GC_push_one ((word) thread_table[i].context.IntT0);
	  GC_push_one ((word) thread_table[i].context.IntT1);
	  GC_push_one ((word) thread_table[i].context.IntT2);
	  GC_push_one ((word) thread_table[i].context.IntT3);
	  GC_push_one ((word) thread_table[i].context.IntT4);
	  GC_push_one ((word) thread_table[i].context.IntT5);
	  GC_push_one ((word) thread_table[i].context.IntT6);
	  GC_push_one ((word) thread_table[i].context.IntT7);
	  GC_push_one ((word) thread_table[i].context.IntS0);
	  GC_push_one ((word) thread_table[i].context.IntS1);
	  GC_push_one ((word) thread_table[i].context.IntS2);
	  GC_push_one ((word) thread_table[i].context.IntS3);
	  GC_push_one ((word) thread_table[i].context.IntS4);
	  GC_push_one ((word) thread_table[i].context.IntS5);
	  GC_push_one ((word) thread_table[i].context.IntS6);
	  GC_push_one ((word) thread_table[i].context.IntS7);
	  GC_push_one ((word) thread_table[i].context.IntT8);
	  GC_push_one ((word) thread_table[i].context.IntT9);
	  GC_push_one ((word) thread_table[i].context.IntK0);
	  GC_push_one ((word) thread_table[i].context.IntK1);
	  GC_push_one ((word) thread_table[i].context.IntS8);
	  GC_push_all_stack((char *) thread_table[i].context.IntSp,
			    thread_table[i].stack);
#	else
#	ifdef PPC
	  if (thread_table[i].context.Gpr1 >= (DWORD)thread_table[i].stack
	      || thread_table[i].context.Gpr1 < (DWORD)bottom)
	      ABORT("Thread stack pointer out of range");
	  GC_push_one ((word) thread_table[i].context.Gpr0);
	  /* Gpr1 is stack pointer */
	  /* Gpr2 is global pointer */
	  GC_push_one ((word) thread_table[i].context.Gpr3);
	  GC_push_one ((word) thread_table[i].context.Gpr4);
	  GC_push_one ((word) thread_table[i].context.Gpr5);
	  GC_push_one ((word) thread_table[i].context.Gpr6);
	  GC_push_one ((word) thread_table[i].context.Gpr7);
	  GC_push_one ((word) thread_table[i].context.Gpr8);
	  GC_push_one ((word) thread_table[i].context.Gpr9);
	  GC_push_one ((word) thread_table[i].context.Gpr10);
	  GC_push_one ((word) thread_table[i].context.Gpr11);
	  GC_push_one ((word) thread_table[i].context.Gpr12);
	  /* Gpr13 is reserved for the kernel */
	  GC_push_one ((word) thread_table[i].context.Gpr14);
	  GC_push_one ((word) thread_table[i].context.Gpr15);
	  GC_push_one ((word) thread_table[i].context.Gpr16);
	  GC_push_one ((word) thread_table[i].context.Gpr17);
	  GC_push_one ((word) thread_table[i].context.Gpr18);
	  GC_push_one ((word) thread_table[i].context.Gpr19);
	  GC_push_one ((word) thread_table[i].context.Gpr20);
	  GC_push_one ((word) thread_table[i].context.Gpr21);
	  GC_push_one ((word) thread_table[i].context.Gpr22);
	  GC_push_one ((word) thread_table[i].context.Gpr23);
	  GC_push_one ((word) thread_table[i].context.Gpr24);
	  GC_push_one ((word) thread_table[i].context.Gpr25);
	  GC_push_one ((word) thread_table[i].context.Gpr26);
	  GC_push_one ((word) thread_table[i].context.Gpr27);
	  GC_push_one ((word) thread_table[i].context.Gpr28);
	  GC_push_one ((word) thread_table[i].context.Gpr29);
	  GC_push_one ((word) thread_table[i].context.Gpr30);
	  GC_push_one ((word) thread_table[i].context.Gpr31);
	  GC_push_all_stack((char *) thread_table[i].context.Gpr1,
			    thread_table[i].stack);
#	else
#	ifdef ALPHA
	  if (thread_table[i].context.IntSp >= (DWORD)thread_table[i].stack
	      || thread_table[i].context.IntSp < (DWORD)bottom)
	      ABORT("Thread stack pointer out of range");
	  GC_push_one ((word) thread_table[i].context.IntV0);
	  GC_push_one ((word) thread_table[i].context.IntT0);
	  GC_push_one ((word) thread_table[i].context.IntT1);
	  GC_push_one ((word) thread_table[i].context.IntT2);
	  GC_push_one ((word) thread_table[i].context.IntT3);
	  GC_push_one ((word) thread_table[i].context.IntT4);
	  GC_push_one ((word) thread_table[i].context.IntT5);
	  GC_push_one ((word) thread_table[i].context.IntT6);
	  GC_push_one ((word) thread_table[i].context.IntT7);
	  GC_push_one ((word) thread_table[i].context.IntS0);
	  GC_push_one ((word) thread_table[i].context.IntS1);
	  GC_push_one ((word) thread_table[i].context.IntS2);
	  GC_push_one ((word) thread_table[i].context.IntS3);
	  GC_push_one ((word) thread_table[i].context.IntS4);
	  GC_push_one ((word) thread_table[i].context.IntS5);
	  GC_push_one ((word) thread_table[i].context.IntFp);
	  GC_push_one ((word) thread_table[i].context.IntA0);
	  GC_push_one ((word) thread_table[i].context.IntA1);
	  GC_push_one ((word) thread_table[i].context.IntA2);
	  GC_push_one ((word) thread_table[i].context.IntA3);
	  GC_push_one ((word) thread_table[i].context.IntA4);
	  GC_push_one ((word) thread_table[i].context.IntA5);
	  GC_push_one ((word) thread_table[i].context.IntT8);
	  GC_push_one ((word) thread_table[i].context.IntT9);
	  GC_push_one ((word) thread_table[i].context.IntT10);
	  GC_push_one ((word) thread_table[i].context.IntT11);
	  GC_push_one ((word) thread_table[i].context.IntT12);
	  GC_push_one ((word) thread_table[i].context.IntAt);
	  GC_push_all_stack((char *) thread_table[i].context.IntSp,
			    thread_table[i].stack);
#	else
	      --> architecture not supported
#	endif /* !ALPHA */
#	endif /* !PPC */
#	endif /* !MIPS */
#	endif /* !SHx */
#	endif /* !ARM32 */
#	endif /* !I386 */
      }
    }
}

void GC_get_next_stack(char *start, char **lo, char **hi)
{
    int i;
#   define ADDR_LIMIT (char *)(-1L)
    char * current_min = ADDR_LIMIT;

    for (i = 0; i < MAX_THREADS; i++) {
    	char * s = (char *)thread_table[i].stack;

	if (0 != s && s > start && s < current_min) {
	    current_min = s;
	}
    }
    *hi = current_min;
    if (current_min == ADDR_LIMIT) {
    	*lo = ADDR_LIMIT;
	return;
    }
    *lo = GC_get_lo_stack_addr(current_min);
    if (*lo < start) *lo = start;
}

#if !defined(CYGWIN32)

#if !defined(MSWINCE) && defined(GC_DLL)

/* We register threads from DllMain */

GC_API HANDLE GC_CreateThread(
    LPSECURITY_ATTRIBUTES lpThreadAttributes, 
    DWORD dwStackSize, LPTHREAD_START_ROUTINE lpStartAddress, 
    LPVOID lpParameter, DWORD dwCreationFlags, LPDWORD lpThreadId )
{
    return CreateThread(lpThreadAttributes, dwStackSize, lpStartAddress,
                        lpParameter, dwCreationFlags, lpThreadId);
}

#else /* defined(MSWINCE) || !defined(GC_DLL))  */

/* We have no DllMain to take care of new threads.  Thus we	*/
/* must properly intercept thread creation.			*/

typedef struct {
    HANDLE child_ready_h, parent_ready_h;
    volatile struct thread_entry * entry;
    LPTHREAD_START_ROUTINE start;
    LPVOID param;
} thread_args;

DWORD WINAPI thread_start(LPVOID arg);

HANDLE WINAPI GC_CreateThread(
    LPSECURITY_ATTRIBUTES lpThreadAttributes, 
    DWORD dwStackSize, LPTHREAD_START_ROUTINE lpStartAddress, 
    LPVOID lpParameter, DWORD dwCreationFlags, LPDWORD lpThreadId )
{
    HANDLE thread_h = NULL;
    HANDLE child_ready_h, parent_ready_h;

    int i;
    thread_args args;

    /* allocate thread slot */
    LOCK();
    for (i = 0; i != MAX_THREADS && thread_table[i].in_use; i++)
	;
    if (i != MAX_THREADS) {
	thread_table[i].in_use = TRUE;
    }
    UNLOCK();

    if (i != MAX_THREADS) {

	/* create unnamed unsignalled events */
	if (child_ready_h = CreateEvent(NULL, FALSE, FALSE, NULL)) {
	    if (parent_ready_h = CreateEvent(NULL, FALSE, FALSE, NULL)) {

		/* set up thread arguments */
		args.child_ready_h = child_ready_h;
		args.parent_ready_h = parent_ready_h;
		args.entry = &thread_table[i];
		args.start = lpStartAddress;
		args.param = lpParameter;

		thread_h = CreateThread(lpThreadAttributes,
					dwStackSize, thread_start,
					&args,
					dwCreationFlags & ~CREATE_SUSPENDED,
					lpThreadId);

		if (thread_h) {

		    /* fill in ID and handle; tell child this is done */
		    thread_table[i].id = *lpThreadId;
		    if (!DuplicateHandle(GetCurrentProcess(),
				 	 thread_h,
					 GetCurrentProcess(),
			 		 &thread_table[i].handle,
			  		 0,
					 0,
					 DUPLICATE_SAME_ACCESS)) {
			DWORD last_error = GetLastError();
			GC_printf1("Last error code: %lx\n", last_error);
			ABORT("DuplicateHandle failed");
		    }
		    SetEvent (parent_ready_h);

		    /* wait for child to fill in stack and copy args */
		    WaitForSingleObject (child_ready_h, INFINITE);

		    /* suspend the child if requested */
		    if (dwCreationFlags & CREATE_SUSPENDED)
			SuspendThread (thread_h);

		    /* let child call given function now (or when resumed) */
		    SetEvent (parent_ready_h);

		} else {
		    CloseHandle (parent_ready_h);
		}
	    }
	}

	CloseHandle (child_ready_h);

	if (thread_h == NULL)
	    thread_table[i].in_use = FALSE;

    } else { /* no thread slot found */
	SetLastError (ERROR_TOO_MANY_TCBS);
    }

    return thread_h;
}

static DWORD WINAPI thread_start(LPVOID arg)
{
    DWORD ret = 0;
    thread_args args = *(thread_args *)arg;

    /* wait for parent to fill in ID and handle */
    WaitForSingleObject (args.parent_ready_h, INFINITE);
    ResetEvent (args.parent_ready_h);

    /* fill in stack; tell parent this is done */
    args.entry->stack = GC_get_stack_base();
    SetEvent (args.child_ready_h);

    /* wait for parent to tell us to go (in case it needs to suspend us) */
    WaitForSingleObject (args.parent_ready_h, INFINITE);
    CloseHandle (args.parent_ready_h);

    /* Clear the thread entry even if we exit with an exception.	*/
    /* This is probably pointless, since an uncaught exception is	*/
    /* supposed to result in the process being killed.			*/
#ifndef __GNUC__
    __try {
#endif /* __GNUC__ */
	ret = args.start (args.param);
#ifndef __GNUC__
    } __finally {
#endif /* __GNUC__ */
	LOCK();
	args.entry->stack = 0;
	args.entry->in_use = FALSE;
	      /* cast away volatile qualifier */
	BZERO((void *) &args.entry->context, sizeof(CONTEXT));
	UNLOCK();
#ifndef __GNUC__
    }
#endif /* __GNUC__ */

    return ret;
}
#endif /* !defined(MSWINCE) && !(defined(__MINGW32__) && !defined(_DLL))  */

#endif /* !CYGWIN32 */

#ifdef MSWINCE

typedef struct {
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    LPWSTR lpCmdLine;
    int nShowCmd;
} main_thread_args;

DWORD WINAPI main_thread_start(LPVOID arg);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
		   LPWSTR lpCmdLine, int nShowCmd)
{
    DWORD exit_code = 1;

    main_thread_args args = {
	hInstance, hPrevInstance, lpCmdLine, nShowCmd
    };
    HANDLE thread_h;
    DWORD thread_id;

    /* initialize everything */
    InitializeCriticalSection(&GC_allocate_ml);
    GC_init();

    /* start the main thread */
    thread_h = GC_CreateThread(
	NULL, 0, main_thread_start, &args, 0, &thread_id);

    if (thread_h != NULL)
    {
	WaitForSingleObject (thread_h, INFINITE);
	GetExitCodeThread (thread_h, &exit_code);
	CloseHandle (thread_h);
    }

    GC_deinit();
    DeleteCriticalSection(&GC_allocate_ml);

    return (int) exit_code;
}

DWORD WINAPI main_thread_start(LPVOID arg)
{
    main_thread_args * args = (main_thread_args *) arg;

    return (DWORD) GC_WinMain (args->hInstance, args->hPrevInstance,
			       args->lpCmdLine, args->nShowCmd);
}

# else /* !MSWINCE */

LONG WINAPI GC_write_fault_handler(struct _EXCEPTION_POINTERS *exc_info);

/* threadAttach/threadDetach routines used by both CYGWIN and DLL
 * implementation, since both recieve explicit notification on thread
 * creation/destruction.
 */
static void threadAttach() {
  int i;
  /* It appears to be unsafe to acquire a lock here, since this	*/
  /* code is apparently not preeemptible on some systems.	*/
  /* (This is based on complaints, not on Microsoft's official	*/
  /* documentation, which says this should perform "only simple	*/
  /* inititalization tasks".)					*/
  /* Hence we make do with nonblocking synchronization.		*/

  /* The following should be a noop according to the win32	*/
  /* documentation.  There is empirical evidence that it	*/
  /* isn't.		- HB					*/
# if defined(MPROTECT_VDB)
   if (GC_incremental) SetUnhandledExceptionFilter(GC_write_fault_handler);
# endif
                /* cast away volatile qualifier */
  for (i = 0; InterlockedExchange((LONG*)&thread_table[i].in_use,1) != 0; i++) {
    /* Compare-and-swap would make this cleaner, but that's not 	*/
    /* supported before Windows 98 and NT 4.0.  In Windows 2000,	*/
    /* InterlockedExchange is supposed to be replaced by		*/
    /* InterlockedExchangePointer, but that's not really what I	*/
    /* want here.							*/
    if (i == MAX_THREADS - 1)
      ABORT("too many threads");
  }
  thread_table[i].id = GetCurrentThreadId();
# ifdef CYGWIN32
    thread_table[i].pthread_id = pthread_self();
# endif
  if (!DuplicateHandle(GetCurrentProcess(),
	               GetCurrentThread(),
		       GetCurrentProcess(),
		       (HANDLE*)&thread_table[i].handle,
		       0,
		       0,
		       DUPLICATE_SAME_ACCESS)) {
	DWORD last_error = GetLastError();
	GC_printf1("Last error code: %lx\n", last_error);
	ABORT("DuplicateHandle failed");
  }
  thread_table[i].stack = GC_get_stack_base();
  if (thread_table[i].stack == NULL) 
    ABORT("Failed to find stack base in threadAttach");
  /* If this thread is being created while we are trying to stop	*/
  /* the world, wait here.  Hopefully this can't happen on any	*/
  /* systems that don't allow us to block here.			*/
  while (GC_please_stop) Sleep(20);
}

static void threadDetach(DWORD thread_id) {
  int i;

  LOCK();
  for (i = 0;
       i < MAX_THREADS &&
       (!thread_table[i].in_use || thread_table[i].id != thread_id);
       i++) {}
  if (i >= MAX_THREADS ) {
    WARN("thread %ld not found on detach", (GC_word)thread_id);
  } else {
    thread_table[i].stack = 0;
    thread_table[i].in_use = FALSE;
    CloseHandle(thread_table[i].handle);
      /* cast away volatile qualifier */
    BZERO((void *)&thread_table[i].context, sizeof(CONTEXT));
  }
  UNLOCK();
}

#ifdef CYGWIN32

/* Called by GC_init() - we hold the allocation lock.	*/
void GC_thr_init() {
    if (GC_thr_initialized) return;
    GC_thr_initialized = TRUE;

#if 0
    /* this might already be handled in GC_init... */
    InitializeCriticalSection(&GC_allocate_ml);
#endif

    /* Add the initial thread, so we can stop it.	*/
    threadAttach();
}

struct start_info {
    void *(*start_routine)(void *);
    void *arg;
};

int GC_pthread_join(pthread_t pthread_id, void **retval) {
    int result;
    int i;

#   if DEBUG_CYGWIN_THREADS
      GC_printf3("thread 0x%x(0x%x) is joining thread 0x%x.\n",(int)pthread_self(),
		 GetCurrentThreadId(), (int)pthread_id);
#   endif

    /* Can't do any table lookups here, because thread being joined 
       might not have registered itself yet */

    result = pthread_join(pthread_id, retval);

    LOCK();
    for (i = 0; !thread_table[i].in_use || thread_table[i].pthread_id != pthread_id;
         i++) {
      if (i == MAX_THREADS - 1) {
        GC_printf1("Failed to find thread 0x%x in pthread_join()\n", pthread_id);
        ABORT("thread not found on detach");
      }
    }
    UNLOCK();
    threadDetach(thread_table[i].id);

#   if DEBUG_CYGWIN_THREADS
      GC_printf3("thread 0x%x(0x%x) completed join with thread 0x%x.\n",
		 (int)pthread_self(), GetCurrentThreadId(), (int)pthread_id);
#   endif

    return result;
}

/* Cygwin-pthreads calls CreateThread internally, but it's not
 * easily interceptible by us..
 *   so intercept pthread_create instead
 */
int
GC_pthread_create(pthread_t *new_thread,
		  const pthread_attr_t *attr,
                  void *(*start_routine)(void *), void *arg) {
    int result;
    struct start_info * si;

    if (!GC_is_initialized) GC_init();
    		/* make sure GC is initialized (i.e. main thread is attached) */
    
    /* This is otherwise saved only in an area mmapped by the thread */
    /* library, which isn't visible to the collector.		 */
    si = GC_malloc_uncollectable(sizeof(struct start_info)); 
    if (0 == si) return(EAGAIN);

    si -> start_routine = start_routine;
    si -> arg = arg;

#   if DEBUG_CYGWIN_THREADS
      GC_printf2("About to create a thread from 0x%x(0x%x)\n",(int)pthread_self(),
		      					      GetCurrentThreadId);
#   endif
    result = pthread_create(new_thread, attr, GC_start_routine, si); 

    if (result) { /* failure */
      	GC_free(si);
    } 

    return(result);
}

void * GC_start_routine(void * arg)
{
    struct start_info * si = arg;
    void * result;
    void *(*start)(void *);
    void *start_arg;
    pthread_t pthread_id;
    int i;

#   if DEBUG_CYGWIN_THREADS
      GC_printf2("thread 0x%x(0x%x) starting...\n",(int)pthread_self(),
		      				   GetCurrentThreadId());
#   endif

    /* If a GC occurs before the thread is registered, that GC will	*/
    /* ignore this thread.  That's fine, since it will block trying to  */
    /* acquire the allocation lock, and won't yet hold interesting 	*/
    /* pointers.							*/
    LOCK();
    /* We register the thread here instead of in the parent, so that	*/
    /* we don't need to hold the allocation lock during pthread_create. */
    threadAttach();
    UNLOCK();

    start = si -> start_routine;
    start_arg = si -> arg;
    pthread_id = pthread_self();

    GC_free(si); /* was allocated uncollectable */

    pthread_cleanup_push(GC_thread_exit_proc, pthread_id);
    result = (*start)(start_arg);
    pthread_cleanup_pop(0);

#   if DEBUG_CYGWIN_THREADS
      GC_printf2("thread 0x%x(0x%x) returned from start routine.\n",
		 (int)pthread_self(),GetCurrentThreadId());
#   endif

    LOCK();
    for (i = 0; thread_table[i].pthread_id != pthread_id; i++) {
      if (i == MAX_THREADS - 1)
        ABORT("thread not found on exit");
    }
    thread_table[i].status = result;
    UNLOCK();

    return(result);
}

void GC_thread_exit_proc(void *arg)
{
    pthread_t pthread_id = (pthread_t)arg;
    int i;

#   if DEBUG_CYGWIN_THREADS
      GC_printf2("thread 0x%x(0x%x) called pthread_exit().\n",
		 (int)pthread_self(),GetCurrentThreadId());
#   endif

    LOCK();
    for (i = 0; thread_table[i].pthread_id != pthread_id; i++) {
      if (i == MAX_THREADS - 1)
        ABORT("thread not found on exit");
    }
    UNLOCK();

#if 0
    /* TODO: we need a way to get the exit value after a pthread_exit so we can stash it safely away */
    thread_table[i].status = ???
#endif
}

/* nothing required here... */
int GC_pthread_sigmask(int how, const sigset_t *set, sigset_t *oset) {
  return pthread_sigmask(how, set, oset);
}
int GC_pthread_detach(pthread_t thread) {
  return pthread_detach(thread);
}
#else /* !CYGWIN32 */

/*
 * We avoid acquiring locks here, since this doesn't seem to be preemptable.
 * Pontus Rydin suggests wrapping the thread start routine instead.
 */
#ifdef GC_DLL
BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved)
{
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    InitializeCriticalSection(&GC_allocate_ml);
    GC_init();	/* Force initialization before thread attach.	*/
    /* fall through */
  case DLL_THREAD_ATTACH:
    threadAttach();
    break;

  case DLL_THREAD_DETACH:
    threadDetach(GetCurrentThreadId());
    break;

  case DLL_PROCESS_DETACH:
    {
      int i;

      LOCK();
      for (i = 0; i < MAX_THREADS; ++i)
      {
          if (thread_table[i].in_use)
          {
              thread_table[i].stack = 0;
              thread_table[i].in_use = FALSE;
              CloseHandle(thread_table[i].handle);
              BZERO((void *) &thread_table[i].context, sizeof(CONTEXT));
          }
      }
      UNLOCK();

      GC_deinit();
      DeleteCriticalSection(&GC_allocate_ml);
    }
    break;

  }
  return TRUE;
}
#endif /* GC_DLL */
#endif /* !CYGWIN32 */

# endif /* !MSWINCE */

#endif /* GC_WIN32_THREADS */
