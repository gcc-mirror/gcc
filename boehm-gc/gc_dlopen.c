/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1997 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2000 by Hewlett-Packard Company.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 * Original author: Bill Janssen
 * Heavily modified by Hans Boehm and others
 */

/*
 * This used to be in dyn_load.c.  It was extracted into a separate file
 * to avoid having to link against libdl.{a,so} if the client doesn't call
 * dlopen.  -HB
 */

#include "private/gc_priv.h"

# if defined(GC_PTHREADS) || defined(GC_SOLARIS_THREADS)

# if defined(dlopen) && !defined(GC_USE_LD_WRAP)
    /* To support various threads pkgs, gc.h interposes on dlopen by     */
    /* defining "dlopen" to be "GC_dlopen", which is implemented below.  */
    /* However, both GC_FirstDLOpenedLinkMap() and GC_dlopen() use the   */
    /* real system dlopen() in their implementation. We first remove     */
    /* gc.h's dlopen definition and restore it later, after GC_dlopen(). */
#   undef dlopen
# endif

  /* Make sure we're not in the middle of a collection, and make	*/
  /* sure we don't start any.	Returns previous value of GC_dont_gc.	*/
  /* This is invoked prior to a dlopen call to avoid synchronization	*/
  /* issues.  We can't just acquire the allocation lock, since startup 	*/
  /* code in dlopen may try to allocate.				*/
  /* This solution risks heap growth in the presence of many dlopen	*/
  /* calls in either a multithreaded environment, or if the library	*/
  /* initialization code allocates substantial amounts of GC'ed memory.	*/
  /* But I don't know of a better solution.				*/
  /* This can still deadlock if the client explicitly starts a GC 	*/
  /* during the dlopen.  He shouldn't do that.				*/
  static GC_bool disable_gc_for_dlopen()
  {
    GC_bool result;
    LOCK();
    result = GC_dont_gc;
    while (GC_incremental && GC_collection_in_progress()) {
	GC_collect_a_little_inner(1000);
    }
    GC_dont_gc = TRUE;
    UNLOCK();
    return(result);
  }

  /* Redefine dlopen to guarantee mutual exclusion with	*/
  /* GC_register_dynamic_libraries.			*/
  /* Should probably happen for other operating	systems, too. */

#include <dlfcn.h>

#ifdef GC_USE_LD_WRAP
  void * __wrap_dlopen(const char *path, int mode)
#else
  void * GC_dlopen(path, mode)
  GC_CONST char * path;
  int mode;
#endif
{
    void * result;
    GC_bool dont_gc_save;
    
#   ifndef USE_PROC_FOR_LIBRARIES
      dont_gc_save = disable_gc_for_dlopen();
#   endif
#   ifdef GC_USE_LD_WRAP
      result = (void *)__real_dlopen(path, mode);
#   else
      result = dlopen(path, mode);
#   endif
#   ifndef USE_PROC_FOR_LIBRARIES
      GC_dont_gc = dont_gc_save;
#   endif
    return(result);
}
# endif  /* GC_PTHREADS || GC_SOLARIS_THREADS ... */



