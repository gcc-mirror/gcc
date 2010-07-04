// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10371: Incorrect tree node built in
// finish_non_static_data_member.

struct A
{
    int i;			// { dg-error "non-static" }
};

template <int> struct B
{
    int foo() { return A::i; }	// { dg-error "this location" }
};

template struct B<0>;
