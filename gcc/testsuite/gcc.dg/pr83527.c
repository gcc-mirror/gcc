/* PR debug/83527 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

extern void fn2(void);
extern void fn3(void);
int a, b;
void fn1() {
  int c;
  short d;
  switch (a) {
  case 32800:
    fn2();
  case 32769:
    b = 0;
  case 32771:
  case 32772:
  case 32782:
    fn3();
  }
  if (d || c) {
    do
      ;
    while (0);
  }
}
