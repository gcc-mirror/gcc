// { dg-do assemble  }

// Copyright (C) 1999, 2001 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// simplified from bug report by K. Haley <khaley@bigfoot.com>
// based on analysis by Martin v. Loewis

// [class.dtor]/11: delete must be implicitly checked for
// accessibility only in the definition of virtual destructors,
// implicitly defined or not.

struct foo {
  foo() {}
private:
  void operator delete(void *) {} // { dg-message "" } private
} foo_;

struct bar : foo {
  ~bar() {
    delete this; // { dg-error "" } delete is private
    // An implicit invocation of delete is emitted in destructors, but
    // it should only be checked in virtual destructors
  } // { dg-bogus "" } not virtual
} bar_;

struct baz : foo {
  virtual ~baz() {} // { dg-error "" } delete is private in vdtor
} baz_;

struct bad : baz {} bad_; // { dg-message "" } delete is private in vdtor
