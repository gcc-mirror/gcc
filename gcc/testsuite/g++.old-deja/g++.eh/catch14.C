// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Jakub Jelinek 2 May 2001 <jakub@redhat.com>

// Build don't link:
// Special g++ Options: -O1

void foo();

struct A {
  A (int x) { };
  ~A() {
    try {
      foo ();
    } catch (...) { }
  };
};

struct B;

B *x;

struct B {
  void a();
  void b();
  static B* c() {
    A y = 0;
    return x;
  };
};

void B::a() {
  c()->b();
}
