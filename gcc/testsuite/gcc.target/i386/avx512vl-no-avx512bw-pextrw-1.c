/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f -mavx512vl -mno-avx512bw" } */

/* vpextrw and vpinsrw require AVX512BW to access xmm16-xmm31.  */

typedef __bf16 v8bf __attribute__ ((vector_size (16)));
typedef _Float16 v8hf __attribute__ ((vector_size (16)));

void
f1 (v8bf v, __bf16 *p)
{
  register v8bf x __asm__ ("xmm17") = v;
  __asm__ volatile ("" : "+v" (x));
  *p = x[1];
}

void
f2 (v8bf v, __bf16 *p)
{
  register v8bf x __asm__ ("xmm17") = v;
  __asm__ volatile ("" : "+v" (x));
  *p = x[0];
}

void
f3 (v8hf v, _Float16 *p)
{
  register v8hf x __asm__ ("xmm17") = v;
  __asm__ volatile ("" : "+v" (x));
  *p = x[3];
}

void
f4 (__bf16 *p)
{
  register __bf16 x __asm__ ("xmm17") = *p;
  __asm__ volatile ("" : "+v" (x));
  *p = x;
}

/* { dg-final { scan-assembler-not "vp(?:extr|insr)w\[^\n\]*%xmm(?:1\[6-9\]|2\[0-9\]|3\[01\])" } } */
