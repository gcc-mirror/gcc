/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-Wno-pmf-conversions" } */

#include <stdio.h>

struct Klass
{
  int implementation ();
  int magic ();
  static void *resolver ();
};

int Klass::implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

void *Klass::resolver (void)
{
  int (Klass::*pmf) () = &Klass::implementation;
  
  return (void *)(int (*)(Klass *))(((Klass *)0)->*pmf);
}

int Klass::magic (void) __attribute__ ((ifunc ("_ZN5Klass8resolverEv")));

struct Klassier : Klass
{
};

int main ()
{
  Klassier obj;
  
  return obj.magic () != 0;
}
