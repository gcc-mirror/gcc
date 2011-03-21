/* PR rtl-optimization/48141 */
/* { dg-do compile } */
/* { dg-options "-O" } */

#define A i = 0;
#define B A A A A A A A A A A
#define C B B B B B B B B B B
#define D C C C C C C C C C C
#define E D D D D D D D D D D

int
foo (void)
{
  volatile int i = 0;
  E E E E E E E E E E E
  return 0;
}
