/* { dg-do compile } */
/* { dg-options "-Os -march=x86-64 -mstringop-strategy=vector_loop" } */

void
foo (char *a, int c)
{
  __builtin_memset (a, c, 56);
}
