// Check contract assertion on a vaarg function
// { dg-do run { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=enforce " }
#include <stdio.h>
#include <stdarg.h>
int simple_printf(const char* fmt, ...) pre (fmt != 0) post (r: r > 0)
{
    va_list args;
    va_start(args, fmt);

    while (*fmt != '\0') {
        if (*fmt == 'd') {
            int i = va_arg(args, int);
            printf("%d\n", i);
        } else if (*fmt == 'c') {
            // A 'char' variable will be promoted to 'int'
            // A character literal in C is already 'int' by itself
            int c = va_arg(args, int);
            printf("%c\n", c);
        } else if (*fmt == 'f') {
            double d = va_arg(args, double);
            printf("%f\n", d);
        }
        ++fmt;
    }

    va_end(args);

    return 6;
}

int main()
{
    simple_printf("dcff", 3, 'a', 1.999, 42.5);
}
