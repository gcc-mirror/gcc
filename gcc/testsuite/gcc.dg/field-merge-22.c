/* { dg-do run } */
/* { dg-options "-O2" } */

/* PR tree-optimization/118456 */
/* Check that compares with constants take into account sign/zero extension of
   both the bitfield and of the shifting type.  */

#define shift (__CHAR_BIT__ - 4)

struct S {
  signed char a : shift + 2;
  signed char b : shift + 2;
  short ignore[0];
} s;

__attribute__((noipa)) int
foo (void)
{
  return ((unsigned char) s.a) >> shift == 15
    && ((unsigned char) s.b) >> shift == 0;
}

int
main ()
{
  s.a = -1;
  s.b = 1;
  if (foo () != 1)
    __builtin_abort ();
  return 0;
}
