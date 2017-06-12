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

N::A f2;              // { dg-error "1:invalid use of template-name 'N::A' without an argument list" "" { target c++14_down } }
				// { dg-error "deduction|no match" "" { target c++1z } .-1 }
N::INVALID f3;        // { dg-error "4:'INVALID' in namespace 'N' does not name a type" }
N::C::INVALID f4;     // { dg-error "7:'INVALID' in 'struct N::C' does not name a type" }
N::K f6;              // { dg-error "4:'K' in namespace 'N' does not name a type" }
typename N::A f7;
// { dg-error "13:invalid use of template-name 'N::A' without an argument list" "13" { target *-*-* } .-1 }

struct B
{
  N::A f2;            // { dg-error "3:invalid use of template-name 'N::A' without an argument list" }
  N::INVALID f3;      // { dg-error "6:'INVALID' in namespace 'N' does not name a type" }
  N::C::INVALID f4;   // { dg-error "9:'INVALID' in 'struct N::C' does not name a type" }
  N::K f6;            // { dg-error "6:'K' in namespace 'N' does not name a type" }
  typename N::A f7;
// { dg-error "15:invalid use of template-name 'N::A' without an argument list" "15" { target *-*-* } .-1 }
};

template <int>
struct C
{
  N::A f2;            // { dg-error "3:invalid use of template-name 'N::A' without an argument list" }
  N::INVALID f3;      // { dg-error "6:'INVALID' in namespace 'N' does not name a type" }
  N::C::INVALID f4;   // { dg-error "9:'INVALID' in 'struct N::C' does not name a type" }
  N::K f6;            // { dg-error "6:'K' in namespace 'N' does not name a type" }
  typename N::A f7;   // { dg-error "15:invalid use of template-name 'N::A' without an argument list" }
};
