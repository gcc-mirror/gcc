// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=cpp" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#warning "Printed"  // { dg-error "\"Printed\" .-Wcpp." }
