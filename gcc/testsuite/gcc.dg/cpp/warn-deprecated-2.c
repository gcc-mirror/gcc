// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=deprecated" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#assert x(x)  // { dg-error "'#assert' is a deprecated GCC extension .-Werror=deprecated." }

#if #x(x)     // { dg-error "assertions are a deprecated extension .-Werror=deprecated." }
#endif
