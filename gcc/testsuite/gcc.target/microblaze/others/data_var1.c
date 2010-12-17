/* { dg-final { scan-assembler "\.bss*" } } */
int global;

int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r0" } } */
    return global;
}
