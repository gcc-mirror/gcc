// PR c++/78344
// { dg-do compile { target c++11 } }

alignas(double) int f alignas;         // { dg-error "30:expected '\\('" }
alignas(double) int g alignas(double;  // { dg-error "37:expected '\\)'" }
