// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wundef" }

#if x  // { dg-warning "'x' is not defined, evaluates to '0' .-Wundef." }
#endif
