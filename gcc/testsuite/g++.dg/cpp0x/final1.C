// PR c++/120628
// { dg-do compile { target c++98_only } }

namespace A {
  struct B {};
  struct B final = {};
}
namespace C {
  struct D { D (int, int); };
  struct D final (42, 0);
}
