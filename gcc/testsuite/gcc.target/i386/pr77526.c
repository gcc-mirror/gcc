/* PR target/77526 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Os -fno-forward-propagate -fno-gcse -fno-rerun-cse-after-loop -mstringop-strategy=byte_loop -Wno-psabi" } */

typedef char U __attribute__((vector_size(64)));
typedef __int128 V __attribute__((vector_size(64)));

V
foo (int a, int b, __int128 c, U u)
{
  u = (u >> (u & 7)) | (u << -(u & 7));
  return a + b + c + (V)u;
}
