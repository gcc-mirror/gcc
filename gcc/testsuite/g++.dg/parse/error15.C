// { dg-do compile }
// Contributed by Volker Reichelt <reichelt at gcc dot gnu dot org> 
// PR c++/14008: Improve diagnostic on invalid types in declarators.

namespace N
{ 
  template<int> struct A {};
  struct C {};
  int K;
}

N::A f2;              // { dg-error "without an argument list" }
N::INVALID f3;        // { dg-error "in namespace 'N' does not name a type" }
N::C::INVALID f4;     // { dg-error "in class 'N::C' does not name a type" }
N::K f6;              // { dg-error "in namespace 'N' does not name a type" }
typename N::A f7;     // { dg-error "without an argument list|outside of template" }

struct B
{
  N::A f2;            // { dg-error "without an argument list" }
  N::INVALID f3;      // { dg-error "in namespace 'N' does not name a type" }
  N::C::INVALID f4;   // { dg-error "in class 'N::C' does not name a type" }
  N::K f6;            // { dg-error "in namespace 'N' does not name a type" }
  typename N::A f7;   // { dg-error "without an argument list|outside of template" }
};

template <int>
struct C
{
  N::A f2;            // { dg-error "without an argument list" }
  N::INVALID f3;      // { dg-error "in namespace 'N' does not name a type" }
  N::C::INVALID f4;   // { dg-error "in class 'N::C' does not name a type" }
  N::K f6;            // { dg-error "in namespace 'N' does not name a type" }
  typename N::A f7;   // { dg-error "without an argument list" }
};

// { dg-bogus "" "bogus excess errors in declaration" { xfail *-*-* } 16 }
