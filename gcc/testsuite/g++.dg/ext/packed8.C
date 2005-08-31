// PR c++/18378
// NOTE: This test assumes packed structure layout differs from unpacked
//       structure layout.  This isn't true, e.g., with the default
//       arm-none-elf options.
// { dg-options "-mstructure-size-boundary=8" { target arm-*-* } }

class A
{
public:
  int i;

  A() {}
  A(const A& a) { i = a.i; }
};

class B
{
  A a __attribute__((packed));

public:
  B() {}
  A GetA() { return a; } // { dg-error "" }
};

