// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wno-cpp" }

#warning "Not printed"  // { dg-bogus "." }
