// PR c++/124200
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodule-header -freflection" }
// { dg-module-cmi {} }

namespace ns {
  inline void a() {}
  inline void b() {}
}
