/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-Wno-pmf-conversions" } */

#include <stdio.h>

struct Klass
{
  int a[4];

  int implementation ();
  int magic ();

  typedef int Func (Klass*);

  static Func* resolver ();
};

int Klass::implementation (void)
{
  printf ("'ere I am JH\n");
  return a[0] + a[1] + a[2] + a[3];
}

Klass::Func* Klass::resolver ()
{
  /* GCC guarantees this conversion to be safe and the resulting pointer
     usable to call the member function using ordinary (i.e., non-member)
     function call syntax.  */

  return reinterpret_cast<Func*>(&Klass::implementation);
}

int Klass::magic (void) __attribute__ ((ifunc ("_ZN5Klass8resolverEv")));

int Foo (Klass &obj, int (Klass::*pmf) ())
{
  return (obj.*pmf) ();
}

int main ()
{
  Klass obj;

  obj.a[0] = 1;
  obj.a[1] = 2;
  obj.a[2] = 3;
  obj.a[3] = 4;

  return Foo (obj, &Klass::magic) != 10;
}
