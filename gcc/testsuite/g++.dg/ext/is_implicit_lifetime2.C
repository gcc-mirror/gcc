// PR c++/122690
// { dg-do compile { target c++11 } }

class A {
  int i;
public:
  A () = default;
  A (int i) : i(i) { }
  A (A const &x) : i(x.i) {}
  A (A &&x) : i(x.i) {}
};
class B {
  int i;
protected:
  B () = default;
public:
  B (int i) : i(i) { }
  B (B const &x) : i(x.i) {}
  B (B &&x) : i(x.i) {}
};
class C {
  int i;
private:
  C () = default;
public:
  C (int i) : i(i) { }
  C (C const &x) : i(x.i) {}
  C (C &&x) : i(x.i) {}
};
class D {
  int i;
public:
  D (D const &) = default;
  D () : i(0) {}
  D (int i) : i(i) { }
  D (D &&x) : i(x.i) {}
};
class E {
  int i;
protected:
  E (E const &) = default;
public:
  E () : i(0) {}
  E (int i) : i(i) { }
  E (E &&x) : i(x.i) {}
};
class F {
  int i;
private:
  F (F const &) = default;
public:
  F () : i(0) {}
  F (int i) : i(i) { }
  F (F &&x) : i(x.i) {}
};
class G {
  int i;
public:
  G (G &&) = default;
  G () : i(0) {}
  G (int i) : i(i) { }
  G (const G &x) : i(x.i) {}
};
class H {
  int i;
protected:
  H (H &&) = default;
public:
  H () : i(0) {}
  H (int i) : i(i) { }
  H (const H &x) : i(x.i) {}
};
class I {
  int i;
private:
  I (I &&) = default;
public:
  I () : i(0) {}
  I (int i) : i(i) { }
  I (const I &x) : i(x.i) {}
};

static_assert (__builtin_is_implicit_lifetime (A), "");
static_assert (__builtin_is_implicit_lifetime (B), "");
static_assert (__builtin_is_implicit_lifetime (C), "");
static_assert (__builtin_is_implicit_lifetime (D), "");
static_assert (__builtin_is_implicit_lifetime (E), "");
static_assert (__builtin_is_implicit_lifetime (F), "");
static_assert (__builtin_is_implicit_lifetime (G), "");
static_assert (__builtin_is_implicit_lifetime (H), "");
static_assert (__builtin_is_implicit_lifetime (I), "");
