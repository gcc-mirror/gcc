// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/26693 
// { dg-do compile }

struct C0
{
};

template<class T, class U>
struct C1
{
    typedef C0 TypedefedC0;

    template<class W>
    void foo (TypedefedC0 *, W)
    {
    }

    template<class W>  C1 (W w)
    {
        TypedefedC0 c;
        foo (&c, w);
    }

};
C0 c0;
C1<int, char> c1 (&c0);
