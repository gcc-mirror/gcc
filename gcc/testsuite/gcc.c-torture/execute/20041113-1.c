#include <stdarg.h>

void abort (void);
void exit (int);

void test (int x, ...)
{
    va_list ap;
    int i;
    va_start (ap, x);
    if (va_arg (ap, int) != 1)
	abort ();
    if (va_arg (ap, int) != 2)
	abort ();
    if (va_arg (ap, int) != 3)
	abort ();
    if (va_arg (ap, int) != 4)
	abort ();
}

double a = 40.0;

int main(int argc, char *argv[])
{
    test(0, 1, 2, 3, (int)(a / 10.0));
    exit (0);
}
