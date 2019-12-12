// PR c++/89217
// { dg-do compile { target c++11 } }

struct R {};

struct C
{
    R* begin() const { return &r; }
    R* end() const { return &r; }

    R& r;
};

struct S
{
    void f1() { f2<true>(); }
    R& r;

    template<bool>
    void f2()
    {
        for (auto i : C{r}) {}
    }
};
