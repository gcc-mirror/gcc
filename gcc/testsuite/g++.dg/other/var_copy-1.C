// { dg-options "-std=c++0x" }
// { dg-do compile }

// Test to allow for va_copy with C++0x standard.

#include <cstdarg>

va_list x;
va_list y;

int main ()
{
    va_copy (y, x);
}
