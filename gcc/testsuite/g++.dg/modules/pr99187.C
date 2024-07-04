// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi pr99187 }

export module pr99187;

export struct A { ~A() {} };

export inline void f() {
  static thread_local A a;
}
