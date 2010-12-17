/* { dg-options "-mxl-gp-opt" } */

/* { dg-final { scan-assembler "\.sbss\[^2]+" } } */
typedef int Boolean;
volatile Boolean global = 0;
int testfunc ()
{
/* { dg-final { scan-assembler "\lwi\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r13" } } */
    return global;
}

int main ()
{

}
