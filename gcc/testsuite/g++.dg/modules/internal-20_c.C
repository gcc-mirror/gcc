// PR c++/122994
// { dg-additional-options "-fmodules" }
// { dg-module-cmi !x }

export module x;

static int internal() { return 42; }

auto a
  = []{ return internal(); };  // { dg-error "exposes TU-local" }

auto b = []{
  struct S {
    inline int foo() {  // { dg-error "exposes TU-local" }
      return internal();
    }
  };
  return S{};
}();
