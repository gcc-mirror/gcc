// { dg-do compile { target c++20 } }

// from [class.union.general] p5

union A { int x; int y[4]; };
struct B { A a; };
union C { B b; int k; };
constexpr int f() {
  C c;                  // does not start lifetime of any union member
  c.b.a.y[3] = 4;       // OK, S(c.b.a.y[3]) contains c.b and c.b.a.y;
                        // creates objects to hold union members c.b and c.b.a.y
  return c.b.a.y[3];    // OK, c.b.a.y refers to newly created object (see [basic.life])
}
constexpr int a = f();

struct X { const int a; int b; };
union Y { X x; int k; };// { dg-message "does not implicitly begin its lifetime" }
constexpr int g() {
  Y y = { { 1, 2 } };   // OK, y.x is active union member ([class.mem])
  int n = y.x.a;
  y.k = 4;              // OK, ends lifetime of y.x, y.k is active member of union

  y.x.b = n;            // { dg-error "accessing .* member instead of initialized .* member" }
                        // undefined behavior: y.x.b modified outside its lifetime,
                        // S(y.x.b) is empty because X's default constructor is deleted,
                        // so union member y.x's lifetime does not implicitly start
  return 0;
}
constexpr int b = g();  // { dg-message "in .constexpr. expansion" }
