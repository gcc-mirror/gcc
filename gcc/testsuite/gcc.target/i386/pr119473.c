/* PR target/119473  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapxf -m64 -mvaes" } */

typedef char __v32qi __attribute__ ((__vector_size__(32)));
typedef long long __m256i __attribute__((__vector_size__(32), __aligned__(32)));

typedef union
{
  __v32qi qi[8];
} tmp_u;


void foo ()
{
  register tmp_u *tdst __asm__("%rdx");
  register tmp_u *src1 __asm__("%rcx");
  register tmp_u *src2 __asm__("%r26");

  tdst->qi[0] = __builtin_ia32_vaesdec_v32qi(src1->qi[0], src2->qi[0]);
  tdst->qi[0] = __builtin_ia32_vaesdeclast_v32qi(src1->qi[0], src2->qi[0]);
  tdst->qi[0] = __builtin_ia32_vaesenc_v32qi(src1->qi[0], src2->qi[0]);
  tdst->qi[0] = __builtin_ia32_vaesenclast_v32qi(src1->qi[0], src2->qi[0]);
}

/* { dg-final { scan-assembler-not "\\\(%r26\\\), " } } */
