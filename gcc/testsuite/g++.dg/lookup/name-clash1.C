// { dg-do compile }

// Origin: Jakub Jelinek <jakub@redhat.com>
// PR c++/5402

struct A
{
    struct B {};
};

struct C
{
    typedef int B;
};

struct D : A
{
    struct E : C {};
    struct B {};
};
