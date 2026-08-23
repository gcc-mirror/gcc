// PR c++/127016
// { dg-do compile { target c++14 } }
// Assigning {} to an aggregate with an array member that has a default
// member initializer, where the element type needs a loop to initialize.

struct NonTrivial {
  NonTrivial () {}
  int v = 0;
};

struct Elem {
  NonTrivial n;
  int x{};
};

struct Bug {
  bool flag{true};
  Elem arr[2]{};
};

void
f (Bug *p)
{
  *p = {};
}

void g (Bug);

void
h ()
{
  g ({});
}

Bug
i ()
{
  return {};
}
