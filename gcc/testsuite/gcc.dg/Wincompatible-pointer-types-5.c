/* { dg-do compile } */
/* { dg-options "" } */

void *
f1 (int flag, int *a, long *b)
{
  return flag ? a : b; /* { dg-error "pointer type mismatch in conditional expression \\\[-Wincompatible-pointer-types\\\]" } */
  /* { dg-note "first expression has type 'int \\*'" "" { target *-*-* } .-1 } */
  /* { dg-note "second expression has type 'long int \\*'" "" { target *-*-* } .-2 } */
}
