// { dg-do compile { target float16 } }
// { dg-options "-std=c++23" }

// Ensure that macro __STDCPP_FLOAT16_T__ evaluates to 1 since this in turn
// ensures that common tests for float16_t are executed.

#if __STDCPP_FLOAT16_T__ != 1
# error "Type float16_t is supported for 64-bit targets starting with z10"
#endif
