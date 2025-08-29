/* { dg-do compile } */
/* { dg-options "-Os -mno-sse -mstringop-strategy=vector_loop" } */

void
foo (char *a)
{
  __builtin_memset (a, 0, 56);
}
