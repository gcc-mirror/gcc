// { dg-do compile { target c++11 } }

// Test to allow for va_copy with C++0x standard.

#include <cstdarg>

va_list x;
va_list y;

int main ()
{
    va_copy (y, x);
}
