/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512vl" } */

_Float16
f1 (_Float16 x)
{
  return __builtin_fabsf16 (x);
}

_Float16
f2 (_Float16 x, _Float16 y)
{
  return __builtin_copysignf16 (x, y);
}

_Float16
f3 (_Float16 x)
{
  return -x;
}

_Float16
f4 (_Float16 x, _Float16 y)
{
  return x * __builtin_copysignf16 (1, y);
}


/* { dg-final { scan-assembler-times "vandps\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[^\n\r\]*xmm\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
