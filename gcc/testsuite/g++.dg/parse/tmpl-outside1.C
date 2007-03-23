// PR c++/3792
// Origin: <david.abrahams@rcn.com>
// { dg-do compile }

struct X
{
   template <int i> struct Y {};
};

typedef X::template Y<0> y; // { dg-error "template|invalid" }
