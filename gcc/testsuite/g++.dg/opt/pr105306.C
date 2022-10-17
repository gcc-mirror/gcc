// PR ipa/105306
// { dg-do compile }
// { dg-options "-Ofast" }

#pragma GCC optimize 0
template <typename T> void foo (T);
struct B { ~B () {} };
struct C { B f; };
template <typename> struct E {
  void bar () { foo (g); }
  C g;
};
template class E<char>;
