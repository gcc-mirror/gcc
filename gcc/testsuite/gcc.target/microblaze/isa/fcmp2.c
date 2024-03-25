/* { dg-options "-O3 -mcpu=v10.0 -mhard-float" } */

volatile float f1, f2, f3;

void float_func () 
{
  /* { dg-final { scan-assembler "fcmp\.(lt|ge)\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    if (f2 < f3) 
        print ("lt");
}
