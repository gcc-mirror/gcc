/* PR c/102989 */
/* Test that -funsigned-bitfields doesn't affect _BitInt bit-fields which are always signed.  */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -funsigned-bitfields" } */

struct S { _BitInt(22) a : 7; signed _BitInt(22) b : 7; unsigned _BitInt(22) c : 7; } s;

int
main ()
{
  s.a = -64;
  s.b = -64;
  s.c = -64;
  if (s.a != -64 || s.b != -64 || s.c != 64)
    __builtin_abort ();
}
