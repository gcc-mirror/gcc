// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/18100: Invalid nested type.

struct A
{
    template<int> struct A {};	// { dg-error "same name" }
};

A::A<0> a;			// { dg-error "not a template" }
