// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror=cpp" }

#warning "Printed"  // { dg-error "\"Printed\" .-Wcpp." }
