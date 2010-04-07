// { dg-do preprocess }
// { dg-options "-fdiagnostics-show-option -Werror=cpp" }

#warning "Printed"  // { dg-error "\"Printed\" .-Wcpp." }
