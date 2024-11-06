// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=unused-macros" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#define X X  // { dg-error "9:macro 'X' is not used .-Werror=unused-macros." }
