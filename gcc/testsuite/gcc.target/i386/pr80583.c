/* PR target/80583 */
/* { dg-do compile } */
/* { dg-options "-O0 -mno-avx" } */

typedef int V __attribute__((__vector_size__(32)));
struct S { V a; };

V __attribute__((target ("avx")))
foo (struct S *b)
{
  V x = b->a;
  return x;
}
