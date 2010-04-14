// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=c++-compat" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#define not !  // { dg-error "identifier \"not\" is a special operator name in C\\+\\+ .-Werror=c\\+\\+-compat." }
