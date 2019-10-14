// PR c++/33840
// { dg-do compile }

template<int> struct A
{
  struct {} : 2;   // { dg-error "expected ';' after struct" "expected" }
};
// { dg-error "ISO C.. forbids declaration" "declaration" { target *-*-* } 6 }
// { dg-error "10:ISO C.. prohibits anonymous" "anonymous" { target *-*-* } 6 }
