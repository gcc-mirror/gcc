// Build don't link:
// 
// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Raymond <raymond@magma.magma-da.com>.
// 
// PR c++/47  The parser failed to resolve 'B' in the return type of
// A::C::D::foo.

class A {
public:
  class B;
  class C;
};

class A::B {
};

class A::C {
  class D;
};

class A::C::D {
public:
  B* foo();
};
