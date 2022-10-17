/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512vl" } */

typedef _Float16 v8hf __attribute__ ((__vector_size__ (16)));
typedef _Float16 v16hf __attribute__ ((__vector_size__ (32)));

void
foo1 (_Float16* __restrict pa, _Float16* __restrict pb,
      _Float16* __restrict pc, _Float16* __restrict pd)
{
  for (int i = 0; i != 8; i++)
    pd[i] = pa[i] * pb[i] + pc[i];
}

/* { dg-final { scan-assembler-times "vfmadd132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

void
foo2 (_Float16* __restrict pa, _Float16* __restrict pb,
      _Float16* __restrict pc, _Float16* __restrict pd)
{
    for (int i = 0; i != 8; i++)
    pd[i] = -pa[i] * pb[i] + pc[i];
}

/* { dg-final { scan-assembler-times "vfnmadd132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

void
foo3 (_Float16* __restrict pa, _Float16* __restrict pb,
      _Float16* __restrict pc, _Float16* __restrict pd)
{
  for (int i = 0; i != 8; i++)
    pd[i] = pa[i] * pb[i] - pc[i];
}

/* { dg-final { scan-assembler-times "vfmsub132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

void
foo4 (_Float16* __restrict pa, _Float16* __restrict pb,
      _Float16* __restrict pc, _Float16* __restrict pd)
{
  for (int i = 0; i != 8; i++)
    pd[i] = -pa[i] * pb[i] - pc[i];
}

/* { dg-final { scan-assembler-times "vfnmsub132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */
