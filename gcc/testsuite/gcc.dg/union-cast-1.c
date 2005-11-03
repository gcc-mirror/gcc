/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */
/* A combine of two extensions to C89 are used here.
   First casts to unions is used.
   Second subscripting non lvalue arrays, this is in C99. */

union vx {short f[8]; int v;};
int vec;

void
foo5 (int vec)
{
  ((union vx) vec).f[5] = 1;
}
