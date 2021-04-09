/* PR debug/99230 */
/* { dg-do compile } */
/* { dg-options "-O2 --param logical-op-non-short-circuit=0 -fcompare-debug --param=jump-table-max-growth-ratio-for-speed=5000" } */

extern void fn2 (void);
extern void fn3 (int);
int a, b;
void
fn1 (void)
{
  int c;
  short d;
  switch (a) {
  case 22000:
    fn2 ();
  case 22300:
    b = 0;
  case 22600:
  case 22601:
  case 22900:
    fn3 (1);
  case 20100:
    fn3 (2);
  case 20200:
    fn3 (3);
  case 20300:
    fn3 (4);
  case 20400:
    fn3 (5);
  case 20310:
    fn3 (4);
  case 20410:
    fn3 (5);
  }
  if (d || c) {
    do
      ;
    while (0);
  }
}
