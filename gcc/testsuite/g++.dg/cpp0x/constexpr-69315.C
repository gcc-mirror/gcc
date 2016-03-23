// PR c++/69315
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

// Template instantiation and evaluation for folding within
// finish_function may call finish_function recursively.
// Make sure we don't reject or delay that sort of recursion.

template <bool> struct Iter;

struct Arg {
  Iter<true> begin();
  Iter<true> end();
};

template <bool> struct Iter {
  int operator*();
  Iter operator++();
  template <bool C1, bool C2> friend constexpr bool operator==(Iter<C1>, Iter<C2>);
  template <bool C1, bool C2> friend constexpr bool operator!=(Iter<C1>, Iter<C2>);
};

void func(Arg a) {
  for (auto ch : a) {
    a.begin() == a.end();
  }
}

template <bool C1, bool C2> constexpr bool operator==(Iter<C1>, Iter<C2>) {
  return true;
}

template <bool C1, bool C2> constexpr bool operator!=(Iter<C1> a, Iter<C2> b) {
  return a == b;
}
