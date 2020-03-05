// PR c++/92695
// { dg-do compile { target c++2a } }

struct A {
  virtual int get () = 0;
  virtual int set (A *o) = 0;
};
struct B : A {
  constexpr int get () override { return 10; }
  constexpr int set (A *o) override { a = o; return 20; }
  A *a {};
};
constexpr auto addressof = [] (A &n) { return &n; };
struct C {
  B b;
  A *c { addressof (b) };
  constexpr int add () { return c->set (addressof (b)); }
};
struct D {
  B b[2];
  A *c { addressof (b[0]) };
  constexpr int add () { return c->set (addressof (b[0])); }
};
template <typename T>
constexpr int get () { T f; return f.add (); }
static_assert (get<C> () == 20);
static_assert (get<D> () == 20);
