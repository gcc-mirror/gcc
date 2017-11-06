// PR c++/80287
// { dg-do compile { target c++11 } }
// { dg-options "-g" }

struct A {
  operator long() { return 0; }
} __attribute__((__may_alias__));

struct {
  A ino;
} a;

char b = a.ino;
