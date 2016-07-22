// PR c++/71248
// { dg-do compile { target c++11 } }

struct S
{
    int a;
    static int S::*typeMembers[] = {  // { dg-error "20:in-class initialization" }
        &S::a,
    };
};
