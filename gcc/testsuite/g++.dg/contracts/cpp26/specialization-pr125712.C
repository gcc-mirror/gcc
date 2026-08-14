// PR c++/125712
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

template<typename>
  void f (bool p) pre (p);
template void f<int> (bool);
template<typename>
  void f (bool) {}

template<typename T>
  struct Add
  {
    template<typename U> void f (U) {}
  };
template<> template<typename U>
  void Add<int>::f (U u) pre (u) {}

template<typename T>
  struct Remove
  {
    template<typename U> void f (U u) pre (u) {}
  };
template<> template<typename U>
  void Remove<int>::f (U) {}

template<typename T>
  struct Replace
  {
    template<typename U> void f (U u) pre (u) {}
  };
template<> template<typename U>
  void Replace<int>::f (U u) pre (u) {}

int
main ()
{
  f<int> (true);
  Add<int>{}.f (false);
  Remove<int>{}.f (false);
  Replace<int>{}.f (false);
}

// { dg-output {contract violation in function void Add<T>::f\(U\) \[with U = bool; T = int\] at .*:18: u(\n|\r\n|\r)} }
// { dg-output {\[assertion_kind: pre, semantic: observe, mode: predicate_false, terminating: no\](\n|\r\n|\r)} }
// { dg-output {contract violation in function void Replace<T>::f\(U\) \[with U = bool; T = int\] at .*:34: u(\n|\r\n|\r)} }
// { dg-output {\[assertion_kind: pre, semantic: observe, mode: predicate_false, terminating: no\](\n|\r\n|\r)} }
