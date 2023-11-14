// PR c++/103499
// { dg-module-do compile }
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi pr103499 }

export module pr103499;

export struct base {
  virtual ~base() = default;
};

export struct derived : base {};
