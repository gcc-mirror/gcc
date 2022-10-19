// PR c++/101449
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr101449 }

export module pr101449;

struct X {
  bool b = true;
  constexpr X() { }
  constexpr X(const X&) { }
};

export constexpr X f() { return {}; }
export constexpr bool g(X x) { return x.b; }
