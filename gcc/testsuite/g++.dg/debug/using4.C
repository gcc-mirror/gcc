// PR c++/26256
// { dg-do compile }

struct A
{
    typedef char type;
};

struct B
{
    typedef int type;
};

struct C : A, B
{
    using A::type;
    type f (type);
};

C::type C::f( type )
{
    type c = 'e';
    return c;
}
