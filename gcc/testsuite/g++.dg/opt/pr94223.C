// PR c++/94223
// { dg-do compile }
// { dg-options "-O0 -std=c++2a -fcompare-debug" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include "../cpp1z/init-statement6.C"
