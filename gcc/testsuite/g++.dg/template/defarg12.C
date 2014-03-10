// PR c++/35828
// { dg-do compile { target c++11 } }

template < typename > struct A ;
template < template < typename > class = A >
void test ()
{
        test ();
}

