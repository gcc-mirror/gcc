/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+, " 3 } } */
/* { dg-final { scan-assembler-not "vmovdqu8" } } */
/* { dg-final { scan-assembler-not "vmovdqu16" } } */

typedef char __v32qi __attribute__ ((__vector_size__ (32)));
typedef char __v32qi_u __attribute__ ((__vector_size__ (32),
				       __aligned__ (1)));
typedef short __v16hi __attribute__ ((__vector_size__ (32)));
typedef short __v16hi_u __attribute__ ((__vector_size__ (32),
					   __aligned__ (1)));
typedef _Float16 __v16hf __attribute__ ((__vector_size__ (32)));
typedef _Float16 __v16hf_u __attribute__ ((__vector_size__ (32),
					   __aligned__ (1)));

extern __v32qi_u v1;
extern __v16hi_u v2;
extern __v16hf_u v3;

void
foo (__v32qi x1, __v16hi x2, __v16hf x3)
{
  v1 = x1;
  v2 = x2;
  v3 = x3;
}
