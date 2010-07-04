/* { dg-do run }  */
/* { dg-require-alias "" } */

#include <typeinfo>

struct Klass
{
  int implementation () const;
  int magic () const;
};

int Klass::implementation (void) const
{
  return 0;
}

int Klass::magic () const
  __attribute__ ((alias ("_ZNK5Klass14implementationEv")));

int __attribute__ ((noinline))
  Foo (Klass const *ptr)
{
  if (ptr->magic () != 0)
    return 1;

  if (typeid (*ptr) != typeid (Klass))
    return 2;

  return 0;
}

int main ()
{
  Klass obj;
  
  return Foo (&obj);
}
