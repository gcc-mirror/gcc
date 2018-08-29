// P0614R1
// { dg-do compile }
// { dg-options "-std=c++2a" }

extern void bar (int);

void
fn0 (int n)
{
  int a[] = { 1, 2, 3, 4, 5 };

  /* Don't get confused by the colon here.  */
  for (int i = 0; n > 0 ? true : false; i++)
    bar (i);

  for (int i = n ? 3 : 4; auto x : a)
    bar (x);

  for (int i = n ? ({ a: 3; }) : 4; i < 10; i++)
    bar (i);

  for (int i = n ? ({ L: 3; }) : 4; auto x : a)
    bar (x);

  for (int i = n; auto x : a)
    bar (x);

  for (int i = n ? n ? n : 3 : 3; auto x : a)
    bar (x);

  for (int i = n ? n ? 3 : n ? 3 : 3 : 3; auto x : a)
    bar (x);

  for (int i = [=]{ return n ? 1 : 2; }(); auto x : a)
    bar (x);

  for (int i = [=]{ L2: if (!n) goto L2; else return 2; }(); auto x : a)
    bar (x);

  for (auto x = n ? 1 : 2 : a) // { dg-error "initializer" }
    bar (x);

  for (int i = 1; auto x = n ? 1 : 2 : a) // { dg-error "initializer" }
    bar (x);
}
