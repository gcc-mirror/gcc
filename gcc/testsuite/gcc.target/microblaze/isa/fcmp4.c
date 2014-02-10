/* { dg-options "-O3 -mcpu=v6.00.a -mhard-float" } */

void float_func(float f1, float f2, float f3)
{
  /* { dg-final { scan-assembler "fcmp\.eq\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
  /* { dg-final { scan-assembler "fcmp\.le\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    if(f1==f2 && f1<=f3)
        print ("f1 eq f2 && f1 le f3");
}
