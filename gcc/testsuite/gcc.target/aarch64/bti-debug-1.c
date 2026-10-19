/* { dg-do compile } */
/* PR target/127934 */
/* { dg-options "-O2 -mbranch-protection=standard -g -fno-var-tracking -fcompare-debug" } */

int g (int);

int
f (int x)
{
  int y = g (x);

  return g (y) + 1;
}

/* { dg-final { scan-assembler "hint\t25 // paciasp" } } */
/* { dg-final { scan-assembler-not "hint\t34 // bti c" } } */
