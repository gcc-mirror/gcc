// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=cpp" { target c } }
// { dg-options "-fdiagnostics-show-option -Werror=cpp" { target c++ } }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
#warning "Printed"  // { dg-error "\"Printed\" .-Werror=cpp." }
