// PR c++/117615
// { dg-do "compile" { target c++20 } }

struct Base {
    virtual void doit (int v) const {}
};

struct Derived : Base {
    void doit (int v) const {}
};

using fn_t = void (Base::*)(int) const;

struct Helper {
    fn_t mFn;
    constexpr Helper (auto && fn) : mFn(static_cast<fn_t>(fn)) {}
};

void foo () {
    constexpr Helper h (&Derived::doit);
    constexpr Helper h2 (&Base::doit);
}
