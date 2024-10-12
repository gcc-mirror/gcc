// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=undef" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#if x  // { dg-error "'x' is not defined, evaluates to '0' .-Werror=undef." }
#endif
