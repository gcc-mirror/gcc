#ifdef WIN32_THREADS

#include "gc_priv.h"

#define STRICT
#include <windows.h>

#define MAX_THREADS 64

struct thread_entry {
  DWORD id;
  HANDLE handle;
  void *stack;   /* The cold end of the stack. */
  CONTEXT context;
};

struct thread_entry thread_table[MAX_THREADS];

void GC_stop_world()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack != 0 && thread_table[i].id != thread_id) {
      if (SuspendThread(thread_table[i].handle) == (DWORD)-1)
	ABORT("SuspendThread failed");
    }
}

void GC_start_world()
{
  DWORD thread_id = GetCurrentThreadId();
  int i;
  for (i = 0; i < MAX_THREADS; i++)
    if (thread_table[i].stack != 0 && thread_table[i].id != thread_id) {
      if (ResumeThread(thread_table[i].handle) == (DWORD)-1)
	ABORT("ResumeThread failed");
    }
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

ptr_t GC_get_lo_stack_addr(ptr_t s)
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
	GC_push_all(&i, thread_table[i].stack);
      else {
	thread_table[i].context.ContextFlags
			= (CONTEXT_INTEGER|CONTEXT_CONTROL);
	if (!GetThreadContext(thread_table[i].handle,
	    		      &thread_table[i].context))
	  ABORT("GetThreadContext failed");
	if (thread_table[i].context.Esp >= (DWORD)thread_table[i].stack
	    || thread_table[i].context.Esp < (DWORD)bottom)
	    ABORT("Thread stack pointer out of range");
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
    /* fall through */
  case DLL_THREAD_ATTACH:
    {
      int i;
      LOCK();
      /* The following should be a noop according to the win32	*/
      /* documentation.  There is empirical evidence that it	*/
      /* isn't.		- HB					*/
      if (GC_incremental) SetUnhandledExceptionFilter(GC_write_fault_handler);
      for (i = 0; thread_table[i].stack != 0; i++) {
	if (i == MAX_THREADS - 1)
	  ABORT("too many threads");
      }
      thread_table[i].stack = GC_get_stack_base();
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
      UNLOCK();
    }
    break;
  case DLL_PROCESS_DETACH:
  case DLL_THREAD_DETACH:
    {
      int i;
      DWORD thread_id = GetCurrentThreadId();
      LOCK();
      for (i = 0; thread_table[i].stack == 0 || thread_table[i].id != thread_id; i++)
	if (i == MAX_THREADS - 1)
	  ABORT("thread not found on detach");
      thread_table[i].stack = 0;
      CloseHandle(thread_table[i].handle);
      BZERO(&thread_table[i].context, sizeof(CONTEXT));
      UNLOCK();
    }
    break;
  }
  return TRUE;
}

#endif /* WIN32_THREADS */
