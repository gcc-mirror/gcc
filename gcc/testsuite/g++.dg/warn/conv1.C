// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Apr 2003 <nathan@codesourcery.com>

// PR 10337, unneeded warning

class A {
  public:
  A() {}
};

class B : public A {
  public:
  B() {}
  void operator=(const A& b) {}
  void operator=(const B& b) {}
};

class C {
  public:
  C() {}
  operator B &() { return _b; }
  operator const B &() const { return _b; }
  
  B _b;
};

int main() {
  B b;
  C c;
  b = c;
}
