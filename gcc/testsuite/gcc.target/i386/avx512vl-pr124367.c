/* PR target/124367 */
/* { dg-do assemble { target { { int128 && avx512vl } && masm_intel } } } */
/* { dg-options "-O -mavx512vl -masm=intel" } */

union {
  __int128 a;
  __attribute__((__vector_size__(sizeof (__int128)))) long long b;
} u;

void
foo ()
{
  u.b ^= 0 > u.a | u.b;
}
