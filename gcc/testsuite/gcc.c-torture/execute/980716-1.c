#include <stdarg.h>

void abort (void);
void exit (int);

void
stub(int num, ...)
{
    va_list ap;
    char *end;
    int i;

    for (i = 0; i < 2; i++) {
        va_start(ap, num);
        while ( 1 ) {
            end = va_arg(ap, char *);
            if (!end) break;
        }
        va_end(ap);
    }
}

int
main()
{
    stub(1, "ab", "bc", "cx", (char *)0);
    exit (0);
}

