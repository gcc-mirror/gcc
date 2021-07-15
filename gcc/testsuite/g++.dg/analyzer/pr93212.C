// { dg-do compile { target c++14 } }

#include <iostream>
auto lol()
{
    int aha = 3;
    return [&aha] { // { dg-warning "dereferencing pointer '.*' to within stale stack frame" }
        return aha;
    };
    /* TODO: may be worth special-casing the reporting of dangling
       references from lambdas, to highlight the declaration, and maybe fix
       the wording (it's a reference, not a pointer, for one thing).  */
}

int main()
{
    auto lambda = lol();
    std::cout << lambda() << std::endl;
    return 0;
}
