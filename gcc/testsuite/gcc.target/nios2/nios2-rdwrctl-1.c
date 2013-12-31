/* { dg-do compile } */

volatile int res;

void x ()
{
  __builtin_wrctl (0, res);
  __builtin_wrctl (15, res);
  __builtin_wrctl (31, res);

  res = __builtin_rdctl (0);
  res = __builtin_rdctl (15);
  res = __builtin_rdctl (31);
} 
