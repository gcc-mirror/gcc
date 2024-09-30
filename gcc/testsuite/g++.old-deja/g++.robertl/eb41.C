// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
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

