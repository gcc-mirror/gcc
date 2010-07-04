// { dg-do preprocess }
// { dg-options "-std=gnu99 -fdiagnostics-show-option -Wno-cpp" { target c } }
// { dg-options "-fdiagnostics-show-option -Wno-cpp" { target c++ } }
#warning "Not printed"  // { dg-bogus "." }
