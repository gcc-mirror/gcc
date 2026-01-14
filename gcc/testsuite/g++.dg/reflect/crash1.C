// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct free_entry {
  free_entry *next;
} first_free_entry;

struct A;
struct B {
  A *a;
} BT;

struct A {
  B b;
};
