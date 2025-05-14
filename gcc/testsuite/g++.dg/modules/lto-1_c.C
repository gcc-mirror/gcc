// PR c++/118961
// { dg-module-do link }
// { dg-require-effective-target lto }
// { dg-additional-options "-fmodules -fno-module-lazy -flto" }

#include "lto-1_a.H"

int main() {}
