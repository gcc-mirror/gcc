// { dg-do compile }

// Origin: Sven Bilke <bilkes@mail.nih.gov>
// PR c++/9777

struct A
{
    struct X {};
    struct Y { void X(); };
};

struct B : A
{
    struct Y : A::Y {};
    struct X : A::X {};
};
