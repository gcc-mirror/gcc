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

  typedef int Func (Klass*);

  static Func* resolver ();
};

int Klassier::implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

Klassier::Func* Klassier::resolver ()
{
  /* GCC guarantees this conversion to be safe and the resulting pointer
     usable to call the member function using ordinary (i.e., non-member)
     function call syntax.  */

  return reinterpret_cast<Func*>(&Klassier::implementation);
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
