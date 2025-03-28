// PR c++/118961
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }
// Test explicit instantiations get emitted with LTO

#include "lto-1.h"
template struct S<char>;
template int x<char>;
template int* foo<char>();
