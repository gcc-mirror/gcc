/* PR target/70300 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=amdfam10 -mavx512f" } */

typedef _Complex A __attribute__ ((mode (SC)));
typedef _Complex B __attribute__ ((mode (DC)));
typedef _Complex C __attribute__ ((mode (TC)));

C
foo (A a, B b, C c, A d, B e, C f)
{
  b -= a;
  d += a;
  a += f;
  return a + b + d + e;
}

__attribute__((target ("avx512vl"))) C
bar (A a, B b, C c, A d, B e, C f)
{
  b -= a;
  d += a;
  a += f;
  return a + b + d + e;
}
