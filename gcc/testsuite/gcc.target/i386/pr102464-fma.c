/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ffast-math -ftree-vectorize -mtune=generic -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "vfmadd...ph" 3 } }  */
/* { dg-final { scan-assembler-times "vfmadd...sh" 3 } }  */
/* { dg-final { scan-assembler-times "vfmadd...ps" 2 } }  */
/* { dg-final { scan-assembler-times "vfmadd...ss" 2 } }  */
/* { dg-final { scan-assembler-times "vfmadd...pd" 1 } }  */
/* { dg-final { scan-assembler-times "vfmadd...sd" 1 } }  */

#include<math.h>
#define FOO(TYPE,SUFFIX)						\
  void									\
  foo_vect_##TYPE##SUFFIX (TYPE* __restrict a, TYPE* b, TYPE* c, TYPE* d) \
  {									\
    for (int i = 0; i != 8; i++)					\
      a[i] = fma##SUFFIX (b[i], c[i], d[i]);				\
  }									\
  TYPE									\
  foo_##TYPE##SUFFIX (TYPE b, TYPE c, TYPE d)				\
  {									\
    return fma##l (b, c, d);						\
  }

FOO (_Float16, f);
FOO (_Float16,);
FOO (_Float16, l);

FOO (float,);
FOO (float, l);

FOO (double, l);
