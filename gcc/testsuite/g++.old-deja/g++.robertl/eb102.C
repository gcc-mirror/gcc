// { dg-do run  }
// Error: intenral compiler error on 1998/05/28 snapshot.
#include <stdio.h>
#include <stdlib.h>

void evilRises (void **ptr)
{
    int *pi;

    pi = new int;

    *pi = 0;

    *ptr = (void *)pi;
}

int main (int argc, char *argv[])
{
#ifdef WORKAROUND
    union foo
#else
    union
#endif
    {
        int a;
        int b;
        int c;
    } *fred, barney;

    evilRises((void **)&fred);

    barney = *fred;

    return EXIT_SUCCESS;
}
