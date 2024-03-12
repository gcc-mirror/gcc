// { dg-do compile }
// { dg-options "-Wmismatched-special-enum" }

enum __c_longlong : byte; // { dg-warning "differ from its declared size" }
