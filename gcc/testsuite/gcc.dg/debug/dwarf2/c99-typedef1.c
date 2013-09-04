// { dg-options "-std=iso9899:1999 -gdwarf" }

void f() {
  int n = 3;
  typedef int T[n++];
  
  T t;
  t[0] = 7;
}
