// PR c++/101833
// { dg-do compile }
// { dg-options "-Wall" }

class Base {};

struct VDerived : virtual Base {
  VDerived(int, int, const char *, ...) __attribute__((format(printf, 2, 3))); // { dg-error ".format. attribute argument 2 value .2. refers to parameter type .int." }
  VDerived(int, const char *, ...) __attribute__((format(printf, 5, 6))); // { dg-warning ".format. attribute argument 2 value .5. exceeds" }
} a(1, "%s %d", "foo", 1);

struct Derived : Base {
  Derived(int, int, const char *, ...) __attribute__((format(printf, 2, 3))); // { dg-error ".format. attribute argument 2 value .2. refers to parameter type .int." }
  Derived(int, const char *, ...) __attribute__((format(printf, 5, 6))); // { dg-warning ".format. attribute argument 2 value .5. exceeds" }
} b(1, "%s %d", "foo", 1);
