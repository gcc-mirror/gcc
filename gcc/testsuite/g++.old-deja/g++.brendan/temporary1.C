// Build don't link: 
// GROUPS passed temporaries
#include <stdio.h>

main ()
{
        int a = 2;

        if (----a == 0)
                printf ("a = 0\n");

        printf ("a = %d\n", a);
}
