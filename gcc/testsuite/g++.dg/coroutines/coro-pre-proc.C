// Only need to compile this, with the default options from the .exp.

#ifndef __cpp_coroutines
#error "coroutines should engaged."
#endif

#if __cpp_coroutines != 201803L
#error "coroutine version out of sync."
#endif
