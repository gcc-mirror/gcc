// PR c++/35828
// { dg-options "-std=c++0x" }

template < typename > struct A ;
template < template < typename > class = A >
void test ()
{
        test ();
}

