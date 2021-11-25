// PR c++/103401
// { dg-do compile { target c++23 } }

void f1 (decltype(auto(0)));
void f2 (decltype(auto{0}));
void f3 (int = auto(42));
void f4 (int = auto{42});
void f5 (decltype(auto(0)) = auto(42));
void f6 (auto (x));
void f7 (int[auto(10)]);
void f8 (int[auto{10}]);
void f9 (auto[auto{10}]);
void f10 (auto);
void f11 (int x, decltype(x) y);
void f12 (int[sizeof(auto{10})]);
void f13 (int[sizeof(auto(10))]);
void f14 (int[__extension__ alignof(auto{10})]);
void f15 (int[__extension__ alignof(auto(10))]);

void
g ()
{
  int a[2];
  f1 (1);
  f2 (1);
  f3 ();
  f3 (1);
  f4 ();
  f4 (1);
  f5 ();
  f5 (1);
  f6 ('a');
  f7 (&a[0]);
  f8 (&a[0]);
  f9 (&a[0]);
  f10 (1);
  f11 (1, 2);
  f12 (&a[0]);
  f13 (&a[0]);
  f14 (&a[0]);
  f15 (&a[0]);
}
