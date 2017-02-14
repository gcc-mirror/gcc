/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

extern int g;
extern void foo ();
#define A g = i
#define B g = i
#define DOIT() foo()
#define DOIT2() foo()

void
f (int i)
{
  if (i == 0)
    A;
  else
    B;

  if (i == 1)
    DOIT();
  else
    DOIT2();
}
