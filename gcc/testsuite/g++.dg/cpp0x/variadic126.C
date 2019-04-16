// PR c++/47220
// { dg-do compile { target c++11 } }

template < typename ... > struct A;

struct B : A <			// { dg-error "invalid" "" { target c++17_down } }
{
}; // { dg-error "mismatch|expected" "" { target c++2a } }
