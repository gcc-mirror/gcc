/* { dg-do assemble } */
/* { dg-options "-O1 --save-temps" } */
#pragma GCC push_options
#pragma GCC target ("+nothing+simd,cmodel=small")

int
cal (double a)
{
  double b = 3.2;
  double c = 2.2;
  if ((a + b) != c)
    return 0;
  else
    return 1;
}

#pragma GCC push_options

#pragma GCC target ("cmodel=large")

int
cal2 (double a)
{

  double b = 3.2;
  double c = 2.2;
  if ((a + b) != c)
    return 0;
  else
    return 1;
}

#pragma GCC pop_options

int
cal3 (double a)
{

  double b = 3.2;
  double c = 2.2;
  if ((a + b) != c)
    return 0;
  else
    return 1;
}

/* { dg-final { scan-assembler-times "adrp" 6 } } */
