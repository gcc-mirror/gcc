// { dg-do compile }
// { dg-options "-fnon-call-exceptions -O2 --param=max-completely-peel-loop-nest-depth=0" }

struct B {
  B() { t = 0; }
  ~B();
  int t;
} w3[1][2];
