#ifndef HAVE_DEFINED_STRIDED_H
#define HAVE_DEFINED_STRIDED_H

#include <stdint-gcc.h>
#include <stdbool.h>
#include <stddef.h>

#define DEF_STRIDED_LD_ST_FORM_1(T)                                     \
  void __attribute__((noinline))                                        \
  vec_strided_load_store_##T##_form_1 (T *restrict out, T *restrict in, \
				       long stride, size_t size)        \
  {                                                                     \
    for (size_t i = 0; i < size; i++)                                   \
      out[i * stride] = in[i * stride];                                 \
  }
#define DEF_STRIDED_LD_ST_FORM_1_WRAP(T) DEF_STRIDED_LD_ST_FORM_1(T)
#define RUN_STRIDED_LD_ST_FORM_1(T, out, in, stride, size) \
  vec_strided_load_store_##T##_form_1 (out, in, stride, size)
#define RUN_STRIDED_LD_ST_FORM_1_WRAP(T, out, in, stride, size) \
  RUN_STRIDED_LD_ST_FORM_1(T, out, in, stride, size)

#endif
