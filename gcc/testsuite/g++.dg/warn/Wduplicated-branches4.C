// PR c++/82541
// { dg-do compile }
// { dg-options "-Wduplicated-branches" }

template<int N>
struct AR
{
    char a1[N > 0 ? N : 1]; // { dg-bogus "this condition has identical branches" }
    char a2[N > 0 ? 1 : 1]; // { dg-warning "this condition has identical branches" }
};

int
main ()
{
    AR<1> w;
}
