// { dg-do assemble  }
// GROUPS passed temporaries
#include <stdio.h>

int main ()
{
        int a = 2;

        if (----a == 0)
                printf ("a = 0\n");

        printf ("a = %d\n", a);
}
