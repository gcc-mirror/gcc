/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-Wno-pmf-conversions" } */

#include <stdio.h>

struct Klass
{
  int implementation ();
  int magic ();

  typedef int Func (Klass*);

  static Func* resolver ();
};

int Klass::implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

Klass::Func* Klass::resolver (void)
{
  /* GCC guarantees this conversion to be safe and the resulting pointer
     usable to call the member function using ordinary (i.e., non-member)
     function call syntax.  */

  return reinterpret_cast<Func*>(&Klass::implementation);
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
