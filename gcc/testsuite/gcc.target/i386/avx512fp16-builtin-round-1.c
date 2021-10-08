/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16" } */

_Float16
f1 (_Float16 x)
{
  return __builtin_truncf16 (x);
}

_Float16
f2 (_Float16 x)
{
  return __builtin_floorf16 (x);
}

_Float16
f3 (_Float16 x)
{
  return __builtin_ceilf16 (x);
}

_Float16
f4 (_Float16 x)
{
  return __builtin_roundevenf16 (x);
}

_Float16
f5 (_Float16 x)
{
  return __builtin_rintf16 (x);
}

_Float16
f6 (_Float16 x)
{
  return __builtin_nearbyintf16 (x);
}

/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\\\$11\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\\\$10\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\\\$9\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\\\$8\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\\\$4\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\\\$12\[^\n\r\]*xmm\[0-9\]" 1 } } */
