// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=multichar" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#if 'abc'  // { dg-error "multi-character character constant .-Werror=multichar." }
#endif
