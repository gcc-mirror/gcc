/* { dg-do compile } */
/* { dg-options "-std=gnu90" } */

typedef int V __attribute__((vector_size(4)));

void
foo(void)
{
  (V){ 0 }[0] = 0;
}
