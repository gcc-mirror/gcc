// { dg-do assemble  }
// { dg-options "-O2" }
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Jakub Jelinek 2 May 2001 <jakub@redhat.com>


struct A;

A *foo();

struct A {
  A *a() { try { return foo(); } catch (...) {} }
  void b();
  void c();
};

void A::b() {
  a()->c();
}
