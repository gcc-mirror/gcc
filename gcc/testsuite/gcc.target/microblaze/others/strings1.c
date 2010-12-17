/* { dg-options "-O3" } */

#include <string.h>

/* { dg-final { scan-assembler "\.rodata*" } } */
extern void somefunc (char *);
int testfunc ()
{
    char string2[80];
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r0,.LC*" } } */    
    strcpy (string2, "hello");
    somefunc (string2);
}
