/* { dg-do run } */
/* { dg-options "-std=gnu89" } */

/* In standard C, enumeration constants always have type int.  If they
   are not representables are int, they're ill-formed.  In GNU C, we
   give such ill-formed constructs well-defined meaning.  Make sure it
   works.  */

#include <stdlib.h>

enum foo
{
  foo1   = 0,
  foo2   = 0xffffffffffffffffULL,
  foo3   = 0xf0fffffffffffffeULL
};

int main ()
{
  if (sizeof (enum foo) != sizeof (unsigned long long))
    abort ();
  exit (0);
}
