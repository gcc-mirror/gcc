#include <stdint-gcc.h>

typedef int8_t v1qi __attribute__ ((vector_size (1)));
typedef int8_t v2qi __attribute__ ((vector_size (2)));
typedef int8_t v4qi __attribute__ ((vector_size (4)));
typedef int8_t v8qi __attribute__ ((vector_size (8)));
typedef int8_t v16qi __attribute__ ((vector_size (16)));
typedef int8_t v32qi __attribute__ ((vector_size (32)));
typedef int8_t v64qi __attribute__ ((vector_size (64)));
typedef int8_t v128qi __attribute__ ((vector_size (128)));
typedef int8_t v256qi __attribute__ ((vector_size (256)));
typedef int8_t v512qi __attribute__ ((vector_size (512)));
typedef int8_t v1024qi __attribute__ ((vector_size (1024)));
typedef int8_t v2048qi __attribute__ ((vector_size (2048)));
typedef int8_t v4096qi __attribute__ ((vector_size (4096)));
typedef int16_t v1hi __attribute__ ((vector_size (2)));
typedef int16_t v2hi __attribute__ ((vector_size (4)));
typedef int16_t v4hi __attribute__ ((vector_size (8)));
typedef int16_t v8hi __attribute__ ((vector_size (16)));
typedef int16_t v16hi __attribute__ ((vector_size (32)));
typedef int16_t v32hi __attribute__ ((vector_size (64)));
typedef int16_t v64hi __attribute__ ((vector_size (128)));
typedef int16_t v128hi __attribute__ ((vector_size (256)));
typedef int16_t v256hi __attribute__ ((vector_size (512)));
typedef int16_t v512hi __attribute__ ((vector_size (1024)));
typedef int16_t v1024hi __attribute__ ((vector_size (2048)));
typedef int16_t v2048hi __attribute__ ((vector_size (4096)));
typedef int32_t v1si __attribute__ ((vector_size (4)));
typedef int32_t v2si __attribute__ ((vector_size (8)));
typedef int32_t v4si __attribute__ ((vector_size (16)));
typedef int32_t v8si __attribute__ ((vector_size (32)));
typedef int32_t v16si __attribute__ ((vector_size (64)));
typedef int32_t v32si __attribute__ ((vector_size (128)));
typedef int32_t v64si __attribute__ ((vector_size (256)));
typedef int32_t v128si __attribute__ ((vector_size (512)));
typedef int32_t v256si __attribute__ ((vector_size (1024)));
typedef int32_t v512si __attribute__ ((vector_size (2048)));
typedef int32_t v1024si __attribute__ ((vector_size (4096)));
typedef int64_t v1di __attribute__ ((vector_size (8)));
typedef int64_t v2di __attribute__ ((vector_size (16)));
typedef int64_t v4di __attribute__ ((vector_size (32)));
typedef int64_t v8di __attribute__ ((vector_size (64)));
typedef int64_t v16di __attribute__ ((vector_size (128)));
typedef int64_t v32di __attribute__ ((vector_size (256)));
typedef int64_t v64di __attribute__ ((vector_size (512)));
typedef int64_t v128di __attribute__ ((vector_size (1024)));
typedef int64_t v256di __attribute__ ((vector_size (2048)));
typedef int64_t v512di __attribute__ ((vector_size (4096)));
typedef _Float16 v1hf __attribute__ ((vector_size (2)));
typedef _Float16 v2hf __attribute__ ((vector_size (4)));
typedef _Float16 v4hf __attribute__ ((vector_size (8)));
typedef _Float16 v8hf __attribute__ ((vector_size (16)));
typedef _Float16 v16hf __attribute__ ((vector_size (32)));
typedef _Float16 v32hf __attribute__ ((vector_size (64)));
typedef _Float16 v64hf __attribute__ ((vector_size (128)));
typedef _Float16 v128hf __attribute__ ((vector_size (256)));
typedef _Float16 v256hf __attribute__ ((vector_size (512)));
typedef _Float16 v512hf __attribute__ ((vector_size (1024)));
typedef _Float16 v1024hf __attribute__ ((vector_size (2048)));
typedef _Float16 v2048hf __attribute__ ((vector_size (4096)));
typedef float v1sf __attribute__ ((vector_size (4)));
typedef float v2sf __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));
typedef float v16sf __attribute__ ((vector_size (64)));
typedef float v32sf __attribute__ ((vector_size (128)));
typedef float v64sf __attribute__ ((vector_size (256)));
typedef float v128sf __attribute__ ((vector_size (512)));
typedef float v256sf __attribute__ ((vector_size (1024)));
typedef float v512sf __attribute__ ((vector_size (2048)));
typedef float v1024sf __attribute__ ((vector_size (4096)));
typedef double v1df __attribute__ ((vector_size (8)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));
typedef double v8df __attribute__ ((vector_size (64)));
typedef double v16df __attribute__ ((vector_size (128)));
typedef double v32df __attribute__ ((vector_size (256)));
typedef double v64df __attribute__ ((vector_size (512)));
typedef double v128df __attribute__ ((vector_size (1024)));
typedef double v256df __attribute__ ((vector_size (2048)));
typedef double v512df __attribute__ ((vector_size (4096)));

#define exhaust_vector_regs()                                                  \
  asm volatile("#" ::                                                          \
		 : "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", \
		   "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",     \
		   "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25",     \
		   "v26", "v27", "v28", "v29", "v30", "v31");

#define DEF_OP_VV(PREFIX, NUM, TYPE, OP)                                       \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP c[i];                                                     \
  }

#define DEF_OP_VX(PREFIX, NUM, TYPE, OP)                                       \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE c)            \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP c;                                                        \
  }

#define DEF_OP_VI_M16(PREFIX, NUM, TYPE, OP)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP -16;                                                      \
  }

#define DEF_OP_VI_15(PREFIX, NUM, TYPE, OP)                                    \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP 15;                                                       \
  }

#define DEF_OP_IV_M16(PREFIX, NUM, TYPE, OP)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = -16 OP b[i];                                                      \
  }

#define DEF_OP_IV_15(PREFIX, NUM, TYPE, OP)                                    \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = 15 OP b[i];                                                       \
  }

#define DEF_MINMAX_VV(PREFIX, NUM, TYPE, OP)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP c[i] ? b[i] : c[i];                                       \
  }

#define DEF_MINMAX_VX(PREFIX, NUM, TYPE, OP)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE c)            \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP c ? b[i] : c;                                             \
  }

#define DEF_OP_VI_7(PREFIX, NUM, TYPE, OP)                                     \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP 7;                                                        \
  }

#define DEF_OP_V(PREFIX, NUM, TYPE, OP)                                        \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b)                    \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = OP b[i];                                                          \
  }

#define DEF_CALL_VV(PREFIX, NUM, TYPE, CALL)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = CALL (b[i], c[i]);                                                \
  }

#define DEF_CALL_VX(PREFIX, NUM, TYPE, CALL)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE c)            \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = CALL (b[i], c);                                                   \
  }

#define DEF_CONST(TYPE, VAL, NUM)                                              \
  void const_##TYPE##_##NUM (TYPE *restrict a)                                 \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = VAL;                                                              \
  }

#define DEF_SERIES(TYPE, BASE, STEP, NUM, SUFFIX)                              \
  void series_##TYPE##_##SUFFIX (TYPE *restrict a)                             \
  {                                                                            \
    for (TYPE i = 0; i < NUM; ++i)                                             \
      a[i] = (BASE) + i * (STEP);                                              \
  }
