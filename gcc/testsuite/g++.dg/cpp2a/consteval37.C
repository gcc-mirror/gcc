// PR c++/111106
// { dg-do compile { target c++20 } }

consteval int id (int i) { return i; }

constexpr int f (auto i)
  requires requires { id (i) } // { dg-error "expected" }
{
  return i;
}

void g () {
  f (42);
}

// { dg-excess-errors "" }
