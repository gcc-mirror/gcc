// PR c++/66832
// { dg-options "-std=c++1z -fconcepts" }

template <class T, class U, unsigned N>
  requires requires (T& t, U &u) { t.foo(); u.foo(); }
void foo_all( T (&t)[N], U (&u)[N] ) {
  for(auto& x : t)
      x.foo();
  for(auto& x : u)
      x.foo();
}

struct S {
  void foo() {}
};

int main() {
  S rg[4] {};
  foo_all(rg, rg);
}

