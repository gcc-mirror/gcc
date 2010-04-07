// { dg-do preprocess }
// { dg-options "-fdiagnostics-show-option -Wno-cpp" }

#warning "Not printed"  // { dg-bogus "." }
