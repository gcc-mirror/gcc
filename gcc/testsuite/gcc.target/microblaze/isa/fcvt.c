/* { dg-options "-O3 -mcpu=v6.00.a -mhard-float -mxl-float-convert" } */

int float_func (float f) 
{
  /* { dg-final { scan-assembler "flt\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
   return f;
}


float int_func (int i) 
{
  /* { dg-final { scan-assembler "fint\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
   return i;
}


float uint_func (unsigned int i) 
{
  /* { dg-final { scan-assembler "fint\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
   return i;
}
