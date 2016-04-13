/* { dg-do assemble } */
/* { dg-options "-O1 --save-temps -mno-fix-cortex-a53-843419" } */
#pragma GCC push_options
#pragma GCC target ("+nothing+simd, cmodel=small")

int
cal (float a)
{
  float b = 1.2;
  float c = 2.2;
  if ((a + b) != c)
    return 0;
  else
    return 1;
}

#pragma GCC push_options

#pragma GCC target ("cmodel=large")

int
cal2 (float a)
{

  float b = 1.2;
  float c = 2.2;
  if ((a + b) != c)
    return 0;
  else
    return 1;
}

#pragma GCC pop_options

int
cal3 (float a)
{

  float b = 1.2;
  float c = 2.2;
  if ((a + b) != c)
    return 0;
  else
    return 1;
}

/* { dg-final { scan-assembler-times "adrp" 6 } } */
