// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror -Wno-error=cpp" { target c } }
// { dg-options "-fdiagnostics-show-option -Werror -Wno-error=cpp" { target c++ } }
#warning "Printed"  // { dg-warning "\"Printed\" .-Wcpp." }
