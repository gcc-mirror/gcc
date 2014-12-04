/* PR target/63910 */
/* { dg-do compile } */
/* { dg-options "-O -mstringop-strategy=vector_loop -mavx512f" } */

extern void bar (float *c);

void
foo (void)
{
  float c[1024] = { };
  bar (c);
}
