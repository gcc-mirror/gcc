// PR c++/112410
// { dg-do compile { target c++23 } }

int f1 (auto(int) -> char);
int f2 (auto x);
int f3 (auto);
int f4 (auto(i));

int v1 (auto(42));
int v2 (auto{42});
int e1 (auto{i}); // { dg-error "not declared" }
int i;
int v3 (auto{i});
int v4 (auto(i + 1));
int v5 (auto(+i));
int v6 (auto(i = 4));

int f5 (auto(i));
int f6 (auto());
int f7 (auto(int));
int f8 (auto(f)(int));
int f9 (auto(...) -> char);
// FIXME: ICEs (PR c++/89867)
//int f10 (auto(__attribute__((unused)) i));
int f11 (auto((i)));
int f12 (auto(i[]));
int f13 (auto(*i));
int f14 (auto(*));

int e2 (auto{}); // { dg-error "invalid use of .auto." }
int e3 (auto(i, i)); // { dg-error "invalid use of .auto." }

char bar (int);
char baz ();
char qux (...);

void
g (int i)
{
  f1 (bar);
  f2 (42);
  f3 (42);
  f4 (42);
  f5 (42);
  f6 (baz);
  f7 (bar);
  f8 (bar);
  f9 (qux);
//  f10 (42);
  f11 (42);
  f12 (&i);
  f13 (&i);
  f14 (&i);

  v1 = 1;
  v2 = 2;
  v3 = 3;
  v4 = 4;
  v5 = 5;
  v6 = 6;
}
