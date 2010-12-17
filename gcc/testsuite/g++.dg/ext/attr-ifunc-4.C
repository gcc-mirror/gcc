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
  static void *resolver ();
};

int Klassier::implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

void *Klassier::resolver (void)
{
  int (Klassier::*pmf) () = &Klassier::implementation;
  
  return (void *)(int (*)(Klassier *))(((Klassier *)0)->*pmf);
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
