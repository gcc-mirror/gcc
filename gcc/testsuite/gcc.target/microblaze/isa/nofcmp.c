/* { dg-options "-O3 -mcpu=v10.0" } */

volatile float f1, f2, f3;

void float_func () 
{
    /* { dg-final { scan-assembler-not "fcmp" } } */
    if (f2 <= f3) 
        print ("le");
    else if (f2 == f3) 
        print ("eq");
    else if (f2 < f3) 
        print ("lt");
    else if (f2 > f3) 
        print ("gt");
    else if (f2 >= f3) 
        print ("ge");
    else if (f2 != f3) 
        print ("ne");
    
}
