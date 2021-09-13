// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions --param=max-stores-to-track=2" }

struct A {
  A() : i() {}
  int i;
} *ap2 = new A[3];
