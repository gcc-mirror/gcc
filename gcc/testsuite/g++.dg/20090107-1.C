/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -Wuninitialized -O1" }  */

template <typename T> struct Q1 { typedef int x; };
template <typename T> struct Q2 {
  typename Q1<T>::x f() {
    int k;
    return k; /* { dg-warning "'k' is used uninitialized" }  */
  }
};
int foo() { return Q2<int>().f(); }
