// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.

constexpr int fn () { return 42; }
struct M {
  static consteval void foo () {}
};

consteval { }
consteval { fn (); }
consteval { M::foo (); }
consteval { auto x = fn (); return; }
consteval {
  [](int i) { return i; }(5);
}
auto lam = [] { };
consteval { lam (); }

struct S {
  consteval { }
};

struct S2 {
  consteval { fn(); }
};

class C {
  consteval { }
};

class C2 {
  consteval { M::foo (); }
};

union U {
  consteval { }
};

template<typename>
struct TS {
  consteval { }
};

template<typename... Ts>
struct TS2 {
  consteval {
    (Ts::foo (), ...);
  }
};

TS2<M> ts2;

void
g ()
{
  consteval { }
}

template<typename>
void
tg ()
{
  consteval { }
}

void die ();
constexpr int
bar (int i)
{
  if (i != 42)
    die ();
  return 0;
}

void
foo ()
{
  constexpr int r = 42;
  consteval {
    bar (r);
  }
}
