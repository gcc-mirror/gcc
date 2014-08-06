/* PR rtl-optimization/61801 */
/* { dg-do compile } */
/* { dg-options "-Os -fcompare-debug" } */

int a, c;
int bar (void);
void baz (void);

void
foo (void)
{
  int d;
  if (bar ())
    {
      int e;
      baz ();
      asm volatile ("" : "=a" (e) : "0" (a), "i" (0));
      d = e;
    }
  c = d;
}
