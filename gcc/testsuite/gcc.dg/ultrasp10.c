/* PR target/11965 */
/* Originator: <jk@tools.de> */
/* { dg-do run { target sparc*-*-* } } */
/* { dg-options "-O -mcpu=ultrasparc" } */

/* This used to fail on 32-bit Ultrasparc because GCC emitted
   an invalid shift instruction.  */


static inline unsigned int shift(int n, unsigned int value)
{
  return value << n;
}

unsigned int val = 1;

int main(void)
{
  int i;

  for (i = 0; i < 4; i++)
    val = shift(32, val);

  return 0;
}
