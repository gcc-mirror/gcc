/* { dg-final { scan-assembler "\.rodata*" } } */
const int global = 10;

int testfunc ()
{
    return global;
}
