# ifndef GC_H
#   include "gc.h"
# endif

/*
 * Invoke all remaining finalizers that haven't yet been run.
 * This is needed for strict compliance with the Java standard, 
 * which can make the runtime guarantee that all finalizers are run.
 * This is problematic for several reasons:
 * 1) It means that finalizers, and all methods calle by them,
 *    must be prepared to deal with objects that have been finalized in
 *    spite of the fact that they are still referenced by statically
 *    allocated pointer variables.
 * 1) It may mean that we get stuck in an infinite loop running
 *    finalizers which create new finalizable objects, though that's
 *    probably unlikely.
 * Thus this is not recommended for general use.
 */
void GC_finalize_all();

/*
 * A version of GC_register_finalizer that allows the object to be
 * finalized before the objects it references.  This is again error
 * prone, in that it makes it easy to accidentally reference finalized
 * objects.  Again, recommended only for JVM implementors.
 */
void GC_register_finalizer_no_order(GC_PTR obj,
			       GC_finalization_proc fn, GC_PTR cd,
			       GC_finalization_proc *ofn, GC_PTR * ocd);

void GC_debug_register_finalizer_no_order(GC_PTR obj,
			       GC_finalization_proc fn, GC_PTR cd,
			       GC_finalization_proc *ofn, GC_PTR * ocd);

#ifdef GC_DEBUG
#   define GC_REGISTER_FINALIZER(p, f, d, of, od) \
	GC_debug_register_finalizer_no_order(p, f, d, of, od)
#else
#   define GC_REGISTER_FINALIZER(p, f, d, of, od) \
	GC_register_finalizer_no_order(p, f, d, of, od)
#endif
