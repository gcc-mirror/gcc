// { dg-do preprocess }
// { dg-options "-fdiagnostics-show-option -Werror=cpp" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#warning "Printed"  // { dg-error "\"Printed\" .-Wcpp." }
