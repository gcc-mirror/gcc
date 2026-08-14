// PR c++/125645
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=enforce" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

template<typename... Args>
  void f (Args... args)
    pre (((args) && ...))
    post (((args) && ...)) {}

int main () {
  f<const bool> (true);
}
