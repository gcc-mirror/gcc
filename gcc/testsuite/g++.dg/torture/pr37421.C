/* { dg-do compile } */
/* { dg-require-effective-target int_eq_float } */

#include <stdio.h>
#include <string.h>

inline int
bci (const float &source)
{
 int dest;
 memcpy (&dest, &source, sizeof (dest));
 return dest;
}

inline float
bcf (const int &source)
{
 float dest;
 memcpy (&dest, &source, sizeof (dest));
 return dest;
}

float
Foo ()
{
 const int foo = bci (0.0f);
 int bar = foo;
 const int baz = foo & 1;
 if (!baz && (foo & 2))
   bar = 0;
 return bcf (bar);
}

int
main ()
{
  printf ("Foo() = %f\n", Foo());
  return 0;
}

