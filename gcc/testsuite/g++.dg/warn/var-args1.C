/* { dg-do compile } */

#include <stdarg.h>

void foo(int, ...)
{
    va_list va;
    int i;
    i = va_arg(va, int&); /* { dg-warning "cannot receive objects" } */
}

