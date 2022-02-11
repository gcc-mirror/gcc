/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ffast-math -ftree-vectorize -mtune=generic -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "vmaxph" 3 } }  */
/* { dg-final { scan-assembler-times "vminph" 3 } }  */
/* { dg-final { scan-assembler-times "vmaxsh" 3 } }  */
/* { dg-final { scan-assembler-times "vminsh" 3 } }  */
/* { dg-final { scan-assembler-times "vmaxps" 2 } }  */
/* { dg-final { scan-assembler-times "vminps" 2 } }  */
/* { dg-final { scan-assembler-times "vmaxss" 2 } }  */
/* { dg-final { scan-assembler-times "vminss" 2 } }  */
/* { dg-final { scan-assembler-times "vmaxpd" 1 } }  */
/* { dg-final { scan-assembler-times "vminpd" 1 } }  */
/* { dg-final { scan-assembler-times "vmaxsd" 1 } }  */
/* { dg-final { scan-assembler-times "vminsd" 1 } }  */

#include<math.h>
#define FOO(CODE,TYPE,SUFFIX)						\
  void									\
  foo_vect_##CODE##TYPE##SUFFIX (TYPE* __restrict a, TYPE* b, TYPE* c)	\
  {									\
    for (int i = 0; i != 8; i++)					\
      a[i] = CODE##SUFFIX (b[i], c[i]);					\
  }									\
  TYPE									\
  foo_##CODE##TYPE##SUFFIX (TYPE b, TYPE c)				\
  {									\
    return CODE##l (b, c);						\
  }

FOO (fmax, _Float16, f);
FOO (fmax, _Float16,);
FOO (fmax, _Float16, l);
FOO (fmin, _Float16, f);
FOO (fmin, _Float16,);
FOO (fmin, _Float16, l);

FOO (fmax, float,);
FOO (fmax, float, l);
FOO (fmin, float,);
FOO (fmin, float, l);

FOO (fmax, double, l);
FOO (fmin, double, l);
