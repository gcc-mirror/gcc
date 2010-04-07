// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Werror -Wno-error=cpp" }

#warning "Printed"  // { dg-warning "\"Printed\" .-Wcpp." }
