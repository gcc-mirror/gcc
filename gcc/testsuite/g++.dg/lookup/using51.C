// PR c++/51382
// { dg-do compile }
// { dg-options "-Wno-deprecated" }

template< int Value >
struct Base
{
   enum { b_e = Value };
};

template< typename Type >
struct Derived : Type
{
   Type::b_e;
   enum { d_e = b_e };
};

int v = (int)Derived< Base< 3 > >::d_e;
