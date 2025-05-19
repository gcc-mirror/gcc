// PR c++/119864
// { dg-do assemble }
// { dg-additional-options "-fmodules -fopenmp" }
// { dg-require-effective-target "fopenmp" }

export module M;

int foo();
int x = foo();
