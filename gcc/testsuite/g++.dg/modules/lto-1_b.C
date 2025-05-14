// PR c++/118961
// { dg-require-effective-target lto }
// { dg-additional-options "-fmodules -flto" }

#include "lto-1.h"

S<char> s;
int y = x<char>;
int* p = foo<char>();
