/* { dg-final { scan-assembler "\.data*" } } */
int global = 10;

int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(0|20)" } } */
    return global;
}
