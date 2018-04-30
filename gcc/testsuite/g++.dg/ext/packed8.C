// PR c++/18378
// NOTE: This test assumes packed structure layout differs from unpacked
//       structure layout.

class A
{
public:
  int i;

  A() {}
  A(const A& a) { i = a.i; }
};

class B
{
  A a __attribute__((packed)); // { dg-warning "attribute ignored" "" { target default_packed } }

public:
  B() {}
  A GetA() { return a; } // { dg-error "" "" { target { ! default_packed } } }
};

