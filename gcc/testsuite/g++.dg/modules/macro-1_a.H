// { dg-additional-options "-fmodules-legacy=macro-1_a.H -fmodules-atom" }
// { dg-module-bmi {"macro-1_a.H"} }

#ifndef MACRO_1_H
#define MACRO_1_H
#define foo bar baz
#define kevin(X) stuart (X)
#define stuart(X) bob ("banana", 0 * X) // Yes we have no bananas
#define bob(...) __VA_ARGS__
#endif
