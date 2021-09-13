// PR c++/98441
// { dg-do compile { target c++11 } }

struct a {
    int& mfn();
};

void fn()
{
    int&  (a::*myvar1)(void) = &a::mfn;
    auto& (a::*myvar2)(void) = &a::mfn;
    auto  (a::*myvar3)(void) = &a::mfn;
}
