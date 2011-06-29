// PR c++/33840
// { dg-do compile }

template<int> struct A
{
  struct {} : 2;   // { dg-error "expected ';' after struct" }
};
// { dg-error "ISO C.. forbids declaration" "" { target *-*-* } 6 }
// { dg-error "ISO C.. prohibits anonymous" "" { target *-*-* } 6 }
