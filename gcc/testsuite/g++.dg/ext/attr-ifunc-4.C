/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-Wno-pmf-conversions" } */

#include <stdio.h>

struct Klass
{
  virtual int magic () = 0;
};

struct Klassier : Klass
{
  int implementation ();
  int magic ();

  typedef int (Klass::*MemFuncPtr)();

  static MemFuncPtr resolver ();
};

int Klassier::implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

Klassier::MemFuncPtr Klassier::resolver (void)
{
  return &Klassier::implementation;
}

int Klassier::magic (void) __attribute__ ((ifunc ("_ZN8Klassier8resolverEv")));

int __attribute__ ((weak)) Foo (Klass &base)
{
  return base.magic ();
}

int main ()
{
  Klassier obj;

  return Foo (obj) != 0;
}
