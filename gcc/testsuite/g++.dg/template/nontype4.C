// { dg-do compile }

// Origin: Ivan Godard <igodard@pacbell.net>
//	   Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/13797: ICE invalid nontype template parameter

template <int> struct A
{
    typedef A<0> B;		// { dg-error "previous declaration" }
    template <B> struct B {};	// { dg-error "not a valid type|typedef" }
};

A<0> a;
