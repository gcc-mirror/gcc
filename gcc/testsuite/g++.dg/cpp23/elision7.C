// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// { dg-options "-Wdangling-reference" }

struct X {
  X ();
  X(X&&);
};

X&& rref ();

X&&
f1 (X&& x)
{
  return x;
}

template<typename T> T&&
f2 (T&& x)
{
  return x;
}
template X& f2<X&>(X&);
template X&& f2<X>(X&&);

X&&
f3 ()
{
  X&& x = rref ();
  return x;
}

void
f4 ()
try {
  X x;
  throw x;
} catch (...) { }

void
f5 ()
{
  auto l1 = [](auto x) -> auto { return x; };
  auto &&x1 = l1(X{});
  auto l2 = [](auto x) -> auto& { return x; }; // { dg-error "cannot bind non-const lvalue reference" }
  auto &&x2 = l2(X{});
  auto l3 = [](auto x) -> auto&& { return x; }; // { dg-warning "reference to local" }
  auto &&x3 = l3(X{});
}

constexpr int &
f6 (int &&n)
{
  return n; // { dg-error "cannot bind non-const lvalue reference" }
}

void
do_f6 ()
{
  auto x = f6 (42);
}

template<typename T> auto &
f7 (T &&t)
{
  return t; // { dg-error "cannot bind non-const lvalue reference" }
}

void
do_f7 ()
{
  const int &x = f7 (0); // { dg-warning "dangling reference" }
}
