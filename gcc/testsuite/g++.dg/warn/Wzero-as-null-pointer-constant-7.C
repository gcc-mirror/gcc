// PR c++/70808
// { dg-options "-Wzero-as-null-pointer-constant" }

int* no_warn = {};
decltype( nullptr ) warn = {};
