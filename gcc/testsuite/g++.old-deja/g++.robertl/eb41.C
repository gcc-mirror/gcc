#include <iostream.h>
#include <iterator.h>
#include <string>

ostream_iterator<string> oo(cout);

int main()
{
    *oo = "Hello, ";
    ++oo;
    *oo = "world!\n";
}

