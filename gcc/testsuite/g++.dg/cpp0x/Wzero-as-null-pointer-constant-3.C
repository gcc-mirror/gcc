// PR c++/70808
// { dg-do compile { target c++11 } }
// { dg-options "-Wzero-as-null-pointer-constant" }

int* no_warn = {};
decltype( nullptr ) warn = {};	// { dg-bogus "zero as null pointer constant" }
