// PR debug/83084
// { dg-do compile }
// { dg-options "-O2 -fcompare-debug -Wno-return-type" }
// { dg-xfail-if "" { powerpc-ibm-aix* } }

enum E { F };
template <E = F> struct A {
  bool foo ();
  int b;
};
template <> bool A<>::foo () {
  int a;
  do
    if (a)
      return false;
  while (__atomic_compare_exchange_n (&b, &a, 0, 1, 4, 0));
}
