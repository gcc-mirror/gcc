#include <stdarg.h>

void foo(va_list v)
{
    unsigned long long x = va_arg (v, unsigned long long);
    if (x != 16LL)
	abort();
}

void bar(char c, char d, ...)
{
    va_list v;
    va_start(v, d);
    foo(v);
    va_end(v);
}

int main(void)
{
    bar(0, 0, 16LL);
    exit(0);
}
