// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/42251
// { dg-do compile }

struct foo
{
    static const bool b = false;
};

template<bool x>
struct S1
{
};

template<bool x>
struct S2
    : S1<foo::b>
{
};

