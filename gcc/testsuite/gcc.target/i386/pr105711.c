/* { dg-do compile } */
/* { dg-options "-O2 --param=sccvn-max-alias-queries-per-access=0" } */

int *p, a, b;

void
foo (_Complex char c)
{
  c /= 3040;
  a %= __builtin_memcmp (1 + &c, p, 1);
  b = c + __imag__ c;
}
