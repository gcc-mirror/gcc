// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option" { target c } }
// { dg-options "-fdiagnostics-show-option" { target c++ } }
#warning "Printed"  // { dg-warning "\"Printed\" .-Wcpp." }
