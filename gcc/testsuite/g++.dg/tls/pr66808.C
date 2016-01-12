// PR c++/66808
// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }

template <typename>
class A {
  int *b = foo ();
  int *foo () { static __thread int a; return &a; }
};
A<int> b;
