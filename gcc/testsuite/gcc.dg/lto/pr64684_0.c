/* { dg-lto-do run } */
/* { dg-lto-options { { -O1 -flto } } } */

extern void fn2 (void);
extern int a;

void
fn1 ()
{
  a = -1;
  fn2 ();
  a &= 1;
}
