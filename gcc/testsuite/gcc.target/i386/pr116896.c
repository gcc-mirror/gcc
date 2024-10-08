/* PR middle-end/116896 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att -fno-stack-protector" } */
/* { dg-final { scan-assembler-times "\tjp\t" 2 } } */
/* { dg-final { scan-assembler-not "\tj\[^mp\]\[a-z\]*\t" } } */
/* { dg-final { scan-assembler-times "\tsbb\[bl\]\t\\\$0, " 4 } } */
/* { dg-final { scan-assembler-times "\tseta\t" 4 } } */

signed char
foo (float x, float y)
{
  if (x == y)
    return 0;
  else if (x < y)
    return -1;
  else if (x > y)
    return 1;
  else
    return 2;
}

__attribute__((optimize ("fast-math"))) signed char
bar (float x, float y)
{
  if (x == y)
    return 0;
  else if (x < y)
    return -1;
  else if (x > y)
    return 1;
  else
    return 2;
}

signed char
baz (float x, float y)
{
  if (x == y)
    return 0;
  else if (x < y)
    return -1;
  else if (x > y)
    return 1;
  else
    return -127;
}

__attribute__((optimize ("fast-math"))) signed char
qux (float x, float y)
{
  if (x == y)
    return 0;
  else if (x < y)
    return -1;
  else if (x > y)
    return 1;
  else
    return -127;
}
