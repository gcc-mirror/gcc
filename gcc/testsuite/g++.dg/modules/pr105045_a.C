// PR c++/105045
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr105045 }

export module pr105045;

export template<int T=0, class U> void f(U) { }
