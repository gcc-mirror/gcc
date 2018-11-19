// { dg-module-do run }
// { dg-additional-options "-fmodule-legacy" }
// { dg-module-bmi {"macro-1_a.H"} }

#ifndef MACRO_1_H
#define MACRO_1_H
#define foo bar baz
#define kevin(X) stuart (X)
#define stuart(X) bob ("banana", X) // Yes we have X bananas
#define bob(...) {__VA_ARGS__}

#endif
