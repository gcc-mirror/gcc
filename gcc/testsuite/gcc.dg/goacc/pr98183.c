/* PR middle-end/98183 */
/* { dg-additional-options "-fexceptions -O0" } */

void bar (void);
int x, y;

void
foo (void)
{
#pragma acc data copyout(x)
  {
#pragma acc data copyout(y)
    bar ();
  }
}
