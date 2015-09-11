/* Origin: PR c++/44108 */
/* { dg-options "-Wunused" } */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */

int
foo ()
{
  unsigned int M = 2;
  const unsigned int M_CONST = 2;
  static unsigned int M_STATIC = 2;
  static const unsigned int M_STATIC_CONST = 2;

  char n1[M];
  char n2[M_CONST];
  char n3[M_STATIC];
  char n4[M_STATIC_CONST];

  return sizeof (n1) + sizeof (n2) + sizeof (n3) + sizeof (n4);
}
