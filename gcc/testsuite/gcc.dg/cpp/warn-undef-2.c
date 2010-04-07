// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=undef" }

#if x  // { dg-error "\"x\" is not defined .-Wundef." }
#endif
