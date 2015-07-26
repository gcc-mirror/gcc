// PR c++/18969
// { dg-do compile { target c++14 } }

template <typename T>
struct A
{
    auto *f1 () { return; } // { dg-error "return-statement" }
    auto &f2 () { return; } // { dg-error "return-statement" }

    auto f3 () { return; } // { dg-bogus "return-statement" }
};
