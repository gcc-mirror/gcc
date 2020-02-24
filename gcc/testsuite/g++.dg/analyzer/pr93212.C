// { dg-do compile { target c++14 } }

#include <iostream>
auto lol()
{
    int aha = 3;
    return [&aha] {
        return aha;
    };
}

int main()
{
    auto lambda = lol();
    std::cout << lambda() << std::endl;
    return 0;
}
