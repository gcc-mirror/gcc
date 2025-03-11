/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* PR target/119131 */

typedef __attribute__((__vector_size__ (64))) char C;
typedef __attribute__((__vector_size__ (64))) _Decimal32 D;
int a, b;
_Decimal32 f;
C e;
C c;

void
foo (D d)
{
  d -= *(_Decimal32 *) __builtin_memset (&f, 0, 4);
  b += a;
  if (a)
    b /= 0; /* { dg-warning "division by zero" } */
  c = (C) d + e;
}

void
foo1 (D d)
{
  __builtin_memset (&f, 0, 4);
  d -= *(_Decimal32 *)&f;
  b += a;
  if (a)
    b /= 0;/* { dg-warning "division by zero" } */
  c = (C) d + e;
}
