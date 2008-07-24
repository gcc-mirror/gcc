// { dg-do compile }
// Contributed by Volker Reichelt <reichelt at gcc dot gnu dot org> 
// { dg-options "-fshow-column" }
// PR c++/14008: Improve diagnostic on invalid types in declarators.

namespace N
{ 
  template<int> struct A {};
  struct C {};
  int K;
}

N::A f2;              // { dg-error "1: error: invalid use of template-name 'N::A' without an argument list" }
N::INVALID f3;        // { dg-error "1: error: 'INVALID' in namespace 'N' does not name a type" }
N::C::INVALID f4;     // { dg-error "1: error: 'INVALID' in class 'N::C' does not name a type" }
N::K f6;              // { dg-error "1: error: 'K' in namespace 'N' does not name a type" }
typename N::A f7;     // { dg-error "1: error: using 'typename' outside of template|13: error: invalid use of template-name 'N::A' without an argument list|17: error: invalid type in declaration before ';' token" }

struct B
{
  N::A f2;            // { dg-error "3: error: invalid use of template-name 'N::A' without an argument list" }
  N::INVALID f3;      // { dg-error "3: error: 'INVALID' in namespace 'N' does not name a type" }
  N::C::INVALID f4;   // { dg-error "3: error: 'INVALID' in class 'N::C' does not name a type" }
  N::K f6;            // { dg-error "3: error: 'K' in namespace 'N' does not name a type" }
  typename N::A f7;   // { dg-error "3: error: using 'typename' outside of template|15: error: invalid use of template-name 'N::A' without an argument list" }
};

template <int>
struct C
{
  N::A f2;            // { dg-error "3: error: invalid use of template-name 'N::A' without an argument list" }
  N::INVALID f3;      // { dg-error "3: error: 'INVALID' in namespace 'N' does not name a type" }
  N::C::INVALID f4;   // { dg-error "3: error: 'INVALID' in class 'N::C' does not name a type" }
  N::K f6;            // { dg-error "3: error: 'K' in namespace 'N' does not name a type" }
  typename N::A f7;   // { dg-error "15: error: invalid use of template-name 'N::A' without an argument list" }
};

// { dg-bogus "bogus excess errors in declaration" "bogus excess errors in declaration" { target *-*-* } 17 }
