/* { dg-do compile } */
/* { dg-options "-mcpu=xt-c920 -mrvv-vector-bits=zvl" } */

int i;
void *p;

void
foo ()
{
  __builtin_memset (p, i, 8);
}
