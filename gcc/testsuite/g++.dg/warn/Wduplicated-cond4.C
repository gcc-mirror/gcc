// PR c++/107593
// { dg-do compile }
// { dg-options "-Wduplicated-cond" }

int n;

template<class T> bool g() { n = 42; return false; }

template<class T>
void f() {
  if (n)
    ;
  else if (g<T>())
    ;
  else if (n)
    ;
}
