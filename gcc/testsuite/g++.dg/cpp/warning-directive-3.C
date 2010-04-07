// { dg-do preprocess }
// { dg-options "-fdiagnostics-show-option -Werror -Wno-error=cpp" }

#warning "Printed"  // { dg-warning "\"Printed\" .-Wcpp." }
