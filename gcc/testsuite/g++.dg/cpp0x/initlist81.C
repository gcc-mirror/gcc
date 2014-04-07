// PR c++/60713
// { dg-options "-O" }
// { dg-do compile { target c++11 } }

template < class x0, class x1, class x2, class x3, class x4 >
int *x5 (x0 *, x2 (x1::*)(x3, x4));

class x6
{
    void x7 ();
    struct x8
    {
        int *x9;
    };
    void x10 (x8);
    void x11 (int *, int *);
};

void
x6::x7 ()
{
    x10 ({
        x5 (this, &x6::x11)
    });
}
