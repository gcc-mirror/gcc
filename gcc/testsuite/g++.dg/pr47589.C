// PR c++/47589
// { dg-do compile }

struct F
{
    typedef void(*Cb)();

    F(Cb);
};

struct C
{
    template<class D> static void f();
};

template<class D>
struct TF : F
{
    TF() : F(C::f<D>) { }
};

struct DTC : TF<DTC>
{
    DTC() { }
};

