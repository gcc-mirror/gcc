// PR c++/13744 (ice-on-valid-code)
// Origin: Thom Harp <thomharp@charter.net>

// { dg-do compile }

template<int> void foo()
{
    struct A
    {
        virtual void bar() { A a(*this); }
    } a;
}

template void foo<0>();
