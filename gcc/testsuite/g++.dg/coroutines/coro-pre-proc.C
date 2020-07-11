// Only need to compile this, with the default options from the .exp.

#ifndef __cpp_impl_coroutine
#error "coroutines should engaged."
#endif

#if __cpp_impl_coroutine != 201902L
#error "coroutine version out of sync."
#endif
