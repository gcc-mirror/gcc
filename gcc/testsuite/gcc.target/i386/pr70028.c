/* PR target/70028 */
/* { dg-do assemble { target avx512bw } } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -fno-forward-propagate -mavx512bw -masm=intel" } */

typedef unsigned short A;
typedef int B __attribute__ ((vector_size (32)));
typedef unsigned __int128 C;
typedef __int128 D __attribute__ ((vector_size (32)));

C
foo (A a, int b, unsigned c, C d, A e, unsigned f, B g, D h)
{
  g[1] ^= (A) ~ a;
  a ^= (unsigned) g[0];
  h %= (D) h | 1;
  return a + b + c + d + e + g[0] + g[1] + h[1];
}
