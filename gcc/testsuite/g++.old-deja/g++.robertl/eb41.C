#include <iostream>
#include <iterator>
#include <string>

std::ostream_iterator<std::string> oo(std::cout);

int main()
{
    *oo = "Hello, ";
    ++oo;
    *oo = "world!\n";
}

