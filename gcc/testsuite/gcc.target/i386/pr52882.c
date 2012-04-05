/* { dg-do compile } */
/* { dg-options "-O" } */

struct S1 {
  int f0;
  int f1;
};

int fn1 ();
void fn2 (struct S1);

void
fn3 () {
  struct S1 a = { 1, 0 };
  if (fn1 ())
    fn2 (a);
  for (; a.f1;) {
  }
}
