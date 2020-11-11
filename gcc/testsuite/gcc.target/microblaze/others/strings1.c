/* { dg-options "-O3" } */
/* { dg-final { scan-assembler "\.rodata*" } } */
/* { dg-final { scan-assembler "addik\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),\\\$LC.*" } } */
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),*" } } */

#include <string.h>

extern void somefunc (char *);
int testfunc ()
{
    char string2[80];
    strcpy (string2, "hello");
    somefunc (string2);
}
