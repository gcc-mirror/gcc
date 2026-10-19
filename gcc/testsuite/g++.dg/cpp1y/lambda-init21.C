// PR c++/95069
// PR c++/104411
// { dg-do compile { target c++14 } }

int main()
{
    auto a = 5;
    const int ca = 5;

    // Direct-list initialization of reference capture (PR c++/95069, PR c++/104411)
    auto l1 = [&r{a}] { return r; };
    auto l2 = [&r{ca}] { return r; };

    // Invalid, can't bind non-const lvalue reference to rvalue
    auto wl = [&r{5}] {};  // { dg-error "cannot bind" }
}
