#ifdef WIN32_THREADS

#include "gc_priv.h"

#if 0
#define STRICT
#include <windows.h>
#endif

#define MAX_THREADS 64

struct thread_entry {
  LONG in_use;
  DWORD id;
  HANDLE handle;
  void *stack;		/* The cold end of the stack.   */
			/* 0 ==> entry not valid.	*/
			/* !in_use ==> stack == 0	*/
  CONTEXT context;
  GC_bool suspended;
};

volatile GC_bool GC_please_stop = FALSE;

volatile struct thread_entry thread_table[MAX_THREADS];

void GC_stop_world()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;

  GC_please_stop = TRUE;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack != 0
	&& thread_table[i].id != thread_id) {
      if (SuspendThread(thread_table[i].handle) == (DWORD)-1)
	ABORT("SuspendThread failed");
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

ptr_t GC_current_stackbottom()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack && thread_table[i].id == thread_id)
      return thread_table[i].stack;
  ABORT("no thread table entry for current thread");
}

static ptr_t GC_get_lo_stack_addr(ptr_t s)
{
    ptr_t bottom;
    MEMORY_BASIC_INFORMATION info;
    VirtualQuery(s, &info, sizeof(info));
    do {
	bottom = info.BaseAddress;
	VirtualQuery(bottom - 1, &info, sizeof(info));
    } while ((info.Protect & PAGE_READWRITE) && !(info.Protect & PAGE_GUARD));
    return(bottom);
}

void GC_push_all_stacks()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack) {
      ptr_t bottom = GC_get_lo_stack_addr(thread_table[i].stack);
      if (thread_table[i].id == thread_id)
	GC_push_all_stack(&i, thread_table[i].stack);
      else {
	thread_table[i].context.ContextFlags
			= (CONTEXT_INTEGER|CONTEXT_CONTROL);
	if (!GetThreadContext(thread_table[i].handle,
	    		      &thread_table[i].context))
	  ABORT("GetThreadContext failed");
	if (thread_table[i].context.Esp >= (DWORD)thread_table[i].stack
	    || thread_table[i].context.Esp < (DWORD)bottom)
	    ABORT("Thread stack pointer out of range");
	GC_push_one ((word) thread_table[i].context.Edi);
    	GC_push_one ((word) thread_table[i].context.Esi);
    	GC_push_one ((word) thread_table[i].context.Ebx);
    	GC_push_one ((word) thread_table[i].context.Edx);
    	GC_push_one ((word) thread_table[i].context.Ecx);
    	GC_push_one ((word) thread_table[i].context.Eax);
	GC_push_all_stack(thread_table[i].context.Esp, thread_table[i].stack);
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

LONG WINAPI GC_write_fault_handler(struct _EXCEPTION_POINTERS *exc_info);

/*
 * This isn't generally safe, since DllMain is not premptible.
 * If another thread holds the lock while this runs we're in trouble.
 * Pontus Rydin suggests wrapping the thread start routine instead.
 */
BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved)
{
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    InitializeCriticalSection(&GC_allocate_ml);
    GC_init();	/* Force initialization before thread attach.	*/
    /* fall through */
  case DLL_THREAD_ATTACH:
    {
      int i;
      /* It appears to be unsafe to acquire a lock here, since this	*/
      /* code is apparently not preeemptible on some systems.		*/
      /* (This is based on complaints, not on Microsoft's official	*/
      /* documentation, which says this should perform "only simple	*/
      /* inititalization tasks".)					*/
      /* Hence we make do with nonblocking synchronization.		*/

      /* The following should be a noop according to the win32	*/
      /* documentation.  There is empirical evidence that it	*/
      /* isn't.		- HB					*/
#     ifndef SMALL_CONFIG
       if (GC_incremental) SetUnhandledExceptionFilter(GC_write_fault_handler);
#     endif

      for (i = 0; InterlockedExchange(&thread_table[i].in_use,1) != 0; i++) {
	/* Compare-and-swap would make this cleaner, but that's not 	*/
	/* supported before Windows 98 and NT 4.0.  In Windows 2000,	*/
	/* InterlockedExchange is supposed to be replaced by		*/
	/* InterlockedExchangePointer, but that's not really what I	*/
	/* want here.							*/
	if (i == MAX_THREADS - 1)
	  ABORT("too many threads");
      }
      thread_table[i].id = GetCurrentThreadId();
      if (!DuplicateHandle(GetCurrentProcess(),
	                   GetCurrentThread(),
			   GetCurrentProcess(),
			   &thread_table[i].handle,
			   0,
			   0,
			   DUPLICATE_SAME_ACCESS)) {
	    DWORD last_error = GetLastError();
	    GC_printf1("Last error code: %lx\n", last_error);
	    ABORT("DuplicateHandle failed");
      }
      thread_table[i].stack = GC_get_stack_base();
      /* If this thread is being created while we are trying to stop	*/
      /* the world, wait here.  Hopefully this can't happen on any	*/
      /* systems that don't allow us to block here.			*/
      while (GC_please_stop) Sleep(20);
    }
    break;
  case DLL_PROCESS_DETACH:
  case DLL_THREAD_DETACH:
    {
      int i;
      DWORD thread_id = GetCurrentThreadId();
      LOCK();
      for (i = 0;
           thread_table[i].stack == 0 || thread_table[i].id != thread_id;
	   i++) {
	if (i == MAX_THREADS - 1)
	  ABORT("thread not found on detach");
      }
      thread_table[i].stack = 0;
      thread_table[i].in_use = FALSE;
      CloseHandle(thread_table[i].handle);
      BZERO(&thread_table[i].context, sizeof(CONTEXT));
      UNLOCK();
    }
    break;
  }
  return TRUE;
}

#endif /* WIN32_THREADS */
