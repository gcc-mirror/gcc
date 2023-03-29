// PR c++/105809
// { dg-do compile { target c++11 } }

template<typename ss> void hh() {  ss t; }

template<int>
int f(void)
{
    constexpr char const* cc = __func__;
    struct j{  char const* kk=cc; };
    hh<j>();
    return 0;
}

int t = f<1>();
