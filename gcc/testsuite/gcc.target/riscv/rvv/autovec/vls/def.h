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
typedef uint64_t v512di __attribute__ ((vector_size (4096)));
typedef uint8_t v1uqi __attribute__ ((vector_size (1)));
typedef uint8_t v2uqi __attribute__ ((vector_size (2)));
typedef uint8_t v4uqi __attribute__ ((vector_size (4)));
typedef uint8_t v8uqi __attribute__ ((vector_size (8)));
typedef uint8_t v16uqi __attribute__ ((vector_size (16)));
typedef uint8_t v32uqi __attribute__ ((vector_size (32)));
typedef uint8_t v64uqi __attribute__ ((vector_size (64)));
typedef uint8_t v128uqi __attribute__ ((vector_size (128)));
typedef uint8_t v256uqi __attribute__ ((vector_size (256)));
typedef uint8_t v512uqi __attribute__ ((vector_size (512)));
typedef uint8_t v1024uqi __attribute__ ((vector_size (1024)));
typedef uint8_t v2048uqi __attribute__ ((vector_size (2048)));
typedef uint8_t v4096uqi __attribute__ ((vector_size (4096)));
typedef uint16_t v1uhi __attribute__ ((vector_size (2)));
typedef uint16_t v2uhi __attribute__ ((vector_size (4)));
typedef uint16_t v4uhi __attribute__ ((vector_size (8)));
typedef uint16_t v8uhi __attribute__ ((vector_size (16)));
typedef uint16_t v16uhi __attribute__ ((vector_size (32)));
typedef uint16_t v32uhi __attribute__ ((vector_size (64)));
typedef uint16_t v64uhi __attribute__ ((vector_size (128)));
typedef uint16_t v128uhi __attribute__ ((vector_size (256)));
typedef uint16_t v256uhi __attribute__ ((vector_size (512)));
typedef uint16_t v512uhi __attribute__ ((vector_size (1024)));
typedef uint16_t v1024uhi __attribute__ ((vector_size (2048)));
typedef uint16_t v2048uhi __attribute__ ((vector_size (4096)));
typedef uint32_t v1usi __attribute__ ((vector_size (4)));
typedef uint32_t v2usi __attribute__ ((vector_size (8)));
typedef uint32_t v4usi __attribute__ ((vector_size (16)));
typedef uint32_t v8usi __attribute__ ((vector_size (32)));
typedef uint32_t v16usi __attribute__ ((vector_size (64)));
typedef uint32_t v32usi __attribute__ ((vector_size (128)));
typedef uint32_t v64usi __attribute__ ((vector_size (256)));
typedef uint32_t v128usi __attribute__ ((vector_size (512)));
typedef uint32_t v256usi __attribute__ ((vector_size (1024)));
typedef uint32_t v512usi __attribute__ ((vector_size (2048)));
typedef uint32_t v1024usi __attribute__ ((vector_size (4096)));
typedef uint64_t v1udi __attribute__ ((vector_size (8)));
typedef uint64_t v2udi __attribute__ ((vector_size (16)));
typedef uint64_t v4udi __attribute__ ((vector_size (32)));
typedef uint64_t v8udi __attribute__ ((vector_size (64)));
typedef uint64_t v16udi __attribute__ ((vector_size (128)));
typedef uint64_t v32udi __attribute__ ((vector_size (256)));
typedef uint64_t v64udi __attribute__ ((vector_size (512)));
typedef uint64_t v128udi __attribute__ ((vector_size (1024)));
typedef uint64_t v256udi __attribute__ ((vector_size (2048)));
typedef uint64_t v512udi __attribute__ ((vector_size (4096)));
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
      a[i] = b[i] OP - 16;                                                     \
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
      a[i] = OP (b[i]);                                                        \
  }

#define DEF_OP_V_CVT(PREFIX, NUM, TYPE_IN, TYPE_OUT, OP)                       \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE_IN##_##TYPE_OUT##_##NUM (TYPE_OUT *restrict a,               \
					   TYPE_IN *restrict b)                \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = OP (b[i]);                                                        \
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

#define DEF_EXTRACT(SCALAR, VECTOR, INDEX)                                     \
  SCALAR                                                                       \
  extract_##SCALAR##VECTOR (VECTOR v)                                          \
  {                                                                            \
    return v[INDEX];                                                           \
  }

#define DEF_MASK_LOGIC(PREFIX, NUM, TYPE, OP)                                  \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c,  \
			TYPE *restrict d, TYPE *restrict e)                    \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = (b[i] > c[i]) OP (d[i] < e[i]);                                   \
  }

#define DEF_SGNJX_VV(PREFIX, NUM, TYPE, CALL)                                  \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)  \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] * CALL (1.0, c[i]);                                          \
  }

#define DEF_REDUC_PLUS(TYPE, NUM)                                              \
  TYPE __attribute__ ((noinline, noclone))                                     \
  reduc_plus_##TYPE##NUM (TYPE *__restrict a)                                  \
  {                                                                            \
    TYPE r = 0;                                                                \
    for (int i = 0; i < NUM; ++i)                                              \
      r += a[i];                                                               \
    return r;                                                                  \
  }

#define DEF_REDUC_MAXMIN(TYPE, NAME, CMP_OP, NUM)                              \
  TYPE __attribute__ ((noinline, noclone))                                     \
  reduc_##NAME##_##TYPE##_##NUM (TYPE *a)                                      \
  {                                                                            \
    TYPE r = 13;                                                               \
    for (int i = 0; i < NUM; ++i)                                              \
      r = a[i] CMP_OP r ? a[i] : r;                                            \
    return r;                                                                  \
  }

#define DEF_REDUC_BITWISE(TYPE, NAME, BIT_OP, NUM)                             \
  TYPE __attribute__ ((noinline, noclone))                                     \
  reduc_##NAME##_##TYPE##_##NUM (TYPE *a)                                      \
  {                                                                            \
    TYPE r = 13;                                                               \
    for (int i = 0; i < NUM; ++i)                                              \
      r BIT_OP a[i];                                                           \
    return r;                                                                  \
  }

#define VARS2(TYPE, NUM1, NUM2) TYPE var##NUM1, TYPE var##NUM2
#define VARS4(TYPE, NUM1, NUM2, NUM3, NUM4)                                    \
  VARS2 (TYPE, NUM1, NUM2), VARS2 (TYPE, NUM3, NUM4)
#define VARS8(TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8)            \
  VARS4 (TYPE, NUM1, NUM2, NUM3, NUM4), VARS4 (TYPE, NUM5, NUM6, NUM7, NUM8)
#define VARS16(TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9,     \
	       NUM10, NUM11, NUM12, NUM13, NUM14, NUM15, NUM16)                \
  VARS8 (TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8),                \
    VARS8 (TYPE, NUM9, NUM10, NUM11, NUM12, NUM13, NUM14, NUM15, NUM16)
#define VARS32(TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9,     \
	       NUM10, NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18,  \
	       NUM19, NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27,  \
	       NUM28, NUM29, NUM30, NUM31, NUM32)                              \
  VARS16 (TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,   \
	  NUM11, NUM12, NUM13, NUM14, NUM15, NUM16),                           \
    VARS16 (TYPE, NUM17, NUM18, NUM19, NUM20, NUM21, NUM22, NUM23, NUM24,      \
	    NUM25, NUM26, NUM27, NUM28, NUM29, NUM30, NUM31, NUM32)
#define VARS64(TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9,     \
	       NUM10, NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18,  \
	       NUM19, NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27,  \
	       NUM28, NUM29, NUM30, NUM31, NUM32, NUM33, NUM34, NUM35, NUM36,  \
	       NUM37, NUM38, NUM39, NUM40, NUM41, NUM42, NUM43, NUM44, NUM45,  \
	       NUM46, NUM47, NUM48, NUM49, NUM50, NUM51, NUM52, NUM53, NUM54,  \
	       NUM55, NUM56, NUM57, NUM58, NUM59, NUM60, NUM61, NUM62, NUM63,  \
	       NUM64)                                                          \
  VARS32 (TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,   \
	  NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19,       \
	  NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28,       \
	  NUM29, NUM30, NUM31, NUM32),                                         \
    VARS32 (TYPE, NUM33, NUM34, NUM35, NUM36, NUM37, NUM38, NUM39, NUM40,      \
	    NUM41, NUM42, NUM43, NUM44, NUM45, NUM46, NUM47, NUM48, NUM49,     \
	    NUM50, NUM51, NUM52, NUM53, NUM54, NUM55, NUM56, NUM57, NUM58,     \
	    NUM59, NUM60, NUM61, NUM62, NUM63, NUM64)
#define VARS128(TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9,    \
		NUM10, NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, \
		NUM19, NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, \
		NUM28, NUM29, NUM30, NUM31, NUM32, NUM33, NUM34, NUM35, NUM36, \
		NUM37, NUM38, NUM39, NUM40, NUM41, NUM42, NUM43, NUM44, NUM45, \
		NUM46, NUM47, NUM48, NUM49, NUM50, NUM51, NUM52, NUM53, NUM54, \
		NUM55, NUM56, NUM57, NUM58, NUM59, NUM60, NUM61, NUM62, NUM63, \
		NUM64, NUM65, NUM66, NUM67, NUM68, NUM69, NUM70, NUM71, NUM72, \
		NUM73, NUM74, NUM75, NUM76, NUM77, NUM78, NUM79, NUM80, NUM81, \
		NUM82, NUM83, NUM84, NUM85, NUM86, NUM87, NUM88, NUM89, NUM90, \
		NUM91, NUM92, NUM93, NUM94, NUM95, NUM96, NUM97, NUM98, NUM99, \
		NUM100, NUM101, NUM102, NUM103, NUM104, NUM105, NUM106,        \
		NUM107, NUM108, NUM109, NUM110, NUM111, NUM112, NUM113,        \
		NUM114, NUM115, NUM116, NUM117, NUM118, NUM119, NUM120,        \
		NUM121, NUM122, NUM123, NUM124, NUM125, NUM126, NUM127,        \
		NUM128)                                                        \
  VARS64 (TYPE, NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,   \
	  NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19,       \
	  NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28,       \
	  NUM29, NUM30, NUM31, NUM32, NUM33, NUM34, NUM35, NUM36, NUM37,       \
	  NUM38, NUM39, NUM40, NUM41, NUM42, NUM43, NUM44, NUM45, NUM46,       \
	  NUM47, NUM48, NUM49, NUM50, NUM51, NUM52, NUM53, NUM54, NUM55,       \
	  NUM56, NUM57, NUM58, NUM59, NUM60, NUM61, NUM62, NUM63, NUM64),      \
    VARS64 (TYPE, NUM65, NUM66, NUM67, NUM68, NUM69, NUM70, NUM71, NUM72,      \
	    NUM73, NUM74, NUM75, NUM76, NUM77, NUM78, NUM79, NUM80, NUM81,     \
	    NUM82, NUM83, NUM84, NUM85, NUM86, NUM87, NUM88, NUM89, NUM90,     \
	    NUM91, NUM92, NUM93, NUM94, NUM95, NUM96, NUM97, NUM98, NUM99,     \
	    NUM100, NUM101, NUM102, NUM103, NUM104, NUM105, NUM106, NUM107,    \
	    NUM108, NUM109, NUM110, NUM111, NUM112, NUM113, NUM114, NUM115,    \
	    NUM116, NUM117, NUM118, NUM119, NUM120, NUM121, NUM122, NUM123,    \
	    NUM124, NUM125, NUM126, NUM127, NUM128)

#define INIT2(NUM1, NUM2) var##NUM1, var##NUM2
#define INIT4(NUM1, NUM2, NUM3, NUM4) INIT2 (NUM1, NUM2), INIT2 (NUM3, NUM4)
#define INIT8(NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8)                  \
  INIT4 (NUM1, NUM2, NUM3, NUM4), INIT4 (NUM5, NUM6, NUM7, NUM8)
#define INIT16(NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,    \
	       NUM11, NUM12, NUM13, NUM14, NUM15, NUM16)                       \
  INIT4 (NUM1, NUM2, NUM3, NUM4), INIT4 (NUM5, NUM6, NUM7, NUM8)
#define INIT32(NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,    \
	       NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19,  \
	       NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28,  \
	       NUM29, NUM30, NUM31, NUM32)                                     \
  INIT16 (NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10, NUM11,  \
	  NUM12, NUM13, NUM14, NUM15, NUM16),                                  \
    INIT16 (NUM17, NUM18, NUM19, NUM20, NUM21, NUM22, NUM23, NUM24, NUM25,     \
	    NUM26, NUM27, NUM28, NUM29, NUM30, NUM31, NUM32)
#define INIT64(NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,    \
	       NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19,  \
	       NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28,  \
	       NUM29, NUM30, NUM31, NUM32, NUM33, NUM34, NUM35, NUM36, NUM37,  \
	       NUM38, NUM39, NUM40, NUM41, NUM42, NUM43, NUM44, NUM45, NUM46,  \
	       NUM47, NUM48, NUM49, NUM50, NUM51, NUM52, NUM53, NUM54, NUM55,  \
	       NUM56, NUM57, NUM58, NUM59, NUM60, NUM61, NUM62, NUM63, NUM64)  \
  INIT32 (NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10, NUM11,  \
	  NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19, NUM20,       \
	  NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28, NUM29,       \
	  NUM30, NUM31, NUM32),                                                \
    INIT32 (NUM33, NUM34, NUM35, NUM36, NUM37, NUM38, NUM39, NUM40, NUM41,     \
	    NUM42, NUM43, NUM44, NUM45, NUM46, NUM47, NUM48, NUM49, NUM50,     \
	    NUM51, NUM52, NUM53, NUM54, NUM55, NUM56, NUM57, NUM58, NUM59,     \
	    NUM60, NUM61, NUM62, NUM63, NUM64)
#define INIT128(NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10,   \
		NUM11, NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19, \
		NUM20, NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28, \
		NUM29, NUM30, NUM31, NUM32, NUM33, NUM34, NUM35, NUM36, NUM37, \
		NUM38, NUM39, NUM40, NUM41, NUM42, NUM43, NUM44, NUM45, NUM46, \
		NUM47, NUM48, NUM49, NUM50, NUM51, NUM52, NUM53, NUM54, NUM55, \
		NUM56, NUM57, NUM58, NUM59, NUM60, NUM61, NUM62, NUM63, NUM64, \
		NUM65, NUM66, NUM67, NUM68, NUM69, NUM70, NUM71, NUM72, NUM73, \
		NUM74, NUM75, NUM76, NUM77, NUM78, NUM79, NUM80, NUM81, NUM82, \
		NUM83, NUM84, NUM85, NUM86, NUM87, NUM88, NUM89, NUM90, NUM91, \
		NUM92, NUM93, NUM94, NUM95, NUM96, NUM97, NUM98, NUM99,        \
		NUM100, NUM101, NUM102, NUM103, NUM104, NUM105, NUM106,        \
		NUM107, NUM108, NUM109, NUM110, NUM111, NUM112, NUM113,        \
		NUM114, NUM115, NUM116, NUM117, NUM118, NUM119, NUM120,        \
		NUM121, NUM122, NUM123, NUM124, NUM125, NUM126, NUM127,        \
		NUM128)                                                        \
  INIT64 (NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8, NUM9, NUM10, NUM11,  \
	  NUM12, NUM13, NUM14, NUM15, NUM16, NUM17, NUM18, NUM19, NUM20,       \
	  NUM21, NUM22, NUM23, NUM24, NUM25, NUM26, NUM27, NUM28, NUM29,       \
	  NUM30, NUM31, NUM32, NUM33, NUM34, NUM35, NUM36, NUM37, NUM38,       \
	  NUM39, NUM40, NUM41, NUM42, NUM43, NUM44, NUM45, NUM46, NUM47,       \
	  NUM48, NUM49, NUM50, NUM51, NUM52, NUM53, NUM54, NUM55, NUM56,       \
	  NUM57, NUM58, NUM59, NUM60, NUM61, NUM62, NUM63, NUM64),             \
    INIT64 (NUM65, NUM66, NUM67, NUM68, NUM69, NUM70, NUM71, NUM72, NUM73,     \
	    NUM74, NUM75, NUM76, NUM77, NUM78, NUM79, NUM80, NUM81, NUM82,     \
	    NUM83, NUM84, NUM85, NUM86, NUM87, NUM88, NUM89, NUM90, NUM91,     \
	    NUM92, NUM93, NUM94, NUM95, NUM96, NUM97, NUM98, NUM99, NUM100,    \
	    NUM101, NUM102, NUM103, NUM104, NUM105, NUM106, NUM107, NUM108,    \
	    NUM109, NUM110, NUM111, NUM112, NUM113, NUM114, NUM115, NUM116,    \
	    NUM117, NUM118, NUM119, NUM120, NUM121, NUM122, NUM123, NUM124,    \
	    NUM125, NUM126, NUM127, NUM128)

#define DEF_INIT(TYPE1, TYPE2, NUM, ...)                                       \
  void init_##TYPE1##_##TYPE2##_##NUM (VARS##NUM (TYPE2, __VA_ARGS__),         \
				       TYPE2 *__restrict out)                  \
  {                                                                            \
    TYPE1 v = {__VA_ARGS__};                                                   \
    *(TYPE1 *) out = v;                                                        \
  }

#define DEF_OP_VV_VA(OP, TYPE1, ...)                                           \
  TYPE1 test_##OP##_##TYPE1 (TYPE1 a, TYPE1 b)                                 \
  {                                                                            \
    return OP (a, b, __VA_ARGS__);                                             \
  }

#define DEF_REPEAT(TYPE1, TYPE2, NUM, ...)                                     \
  void init_##TYPE1##_##TYPE2##_##NUM (TYPE2 var0, TYPE2 var1,                 \
				       TYPE2 *__restrict out)                  \
  {                                                                            \
    TYPE1 v = {__VA_ARGS__};                                                   \
    *(TYPE1 *) out = v;                                                        \
  }

#define DEF_VEC_SET_IMM_INDEX(PREFIX, VECTOR, TYPE, INDEX)                     \
  VECTOR __attribute__ ((noinline, noclone))                                   \
  PREFIX##_##VECTOR##_##INDEX (VECTOR v, TYPE a)                               \
  {                                                                            \
    v[INDEX] = a;                                                              \
                                                                               \
    return v;                                                                  \
  }

#define DEF_VEC_SET_SCALAR_INDEX(PREFIX, VECTOR, TYPE)                         \
  VECTOR __attribute__ ((noinline, noclone))                                   \
  PREFIX##_##VECTOR##_##TYPE (VECTOR v, TYPE a, unsigned index)                \
  {                                                                            \
    v[index] = a;                                                              \
                                                                               \
    return v;                                                                  \
  }

#define DEF_FMA_VV(PREFIX, NUM, TYPE)                                          \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c,  \
			TYPE *restrict d)                                      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] * c[i] + d[i];                                               \
  }

#define DEF_FNMA_VV(PREFIX, NUM, TYPE)                                         \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c,  \
			TYPE *restrict d)                                      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = d[i] - b[i] * c[i];                                               \
  }

#define DEF_FMS_VV(PREFIX, NUM, TYPE)                                          \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c,  \
			TYPE *restrict d)                                      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] * c[i] - d[i];                                               \
  }

#define DEF_FNMS_VV(PREFIX, NUM, TYPE)                                         \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c,  \
			TYPE *restrict d)                                      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = -(b[i] * c[i]) - d[i];                                            \
  }

#define DEF_CONVERT(PREFIX, TYPE1, TYPE2, NUM)                                 \
  __attribute__ ((                                                             \
    noipa)) void PREFIX##_##TYPE1##TYPE2##_##NUM (TYPE2 *__restrict dst,       \
						  TYPE1 *__restrict a)         \
  {                                                                            \
    for (int i = 0; i < NUM; i++)                                              \
      dst[i] = (TYPE2) a[i];                                                   \
  }

#define DEF_AVG_FLOOR(TYPE, TYPE2, NUM)                                        \
  __attribute__ ((noipa)) void vavg_##TYPE##_##TYPE2##NUM (                    \
    TYPE *__restrict dst, TYPE *__restrict a, TYPE *__restrict b, int n)       \
  {                                                                            \
    for (int i = 0; i < NUM; i++)                                              \
      dst[i] = ((TYPE2) a[i] + b[i]) >> 1;                                     \
  }

#define DEF_AVG_CEIL(TYPE, TYPE2, NUM)                                         \
  __attribute__ ((noipa)) void vavg2_##TYPE##_##TYPE2##NUM (                   \
    TYPE *__restrict dst, TYPE *__restrict a, TYPE *__restrict b, int n)       \
  {                                                                            \
    for (int i = 0; i < NUM; i++)                                              \
      dst[i] = ((TYPE2) a[i] + b[i] + 1) >> 1;                                 \
  }

#define DEF_MULH(TYPE, NUM)                                                    \
  void __attribute__ ((noipa))                                                 \
  mod_##TYPE##_##NUM (TYPE *__restrict dst, TYPE *__restrict src)              \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      dst[i] = src[i] % 19;                                                    \
  }

#define DEF_COND_UNOP(PREFIX, NUM, TYPE, OP)                                   \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE cond)                             \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? OP (a[i]) : b[i];                                       \
    return v;                                                                  \
  }

#define DEF_COND_BINOP(PREFIX, NUM, TYPE, OP)                                  \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? a[i] OP b[i] : c[i];                                    \
    return v;                                                                  \
  }

#define DEF_COND_MINMAX(PREFIX, NUM, TYPE, OP)                                 \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? ((a[i]) OP (b[i]) ? (a[i]) : (b[i])) : c[i];            \
    return v;                                                                  \
  }

#define DEF_COND_FMA_VV(PREFIX, NUM, TYPE)                                     \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? a[i] * b[i] + c[i] : b[i];                              \
    return v;                                                                  \
  }

#define DEF_COND_FNMA_VV(PREFIX, NUM, TYPE)                                    \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? a[i] - b[i] * c[i] : b[i];                              \
    return v;                                                                  \
  }

#define DEF_COND_FMS_VV(PREFIX, NUM, TYPE)                                     \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? a[i] * b[i] - c[i] : b[i];                              \
    return v;                                                                  \
  }

#define DEF_COND_FNMS_VV(PREFIX, NUM, TYPE)                                    \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? -(a[i] * b[i]) - c[i] : b[i];                           \
    return v;                                                                  \
  }

#define DEF_OP_WVV(PREFIX, NUM, TYPE, TYPE2, OP)                               \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##_##TYPE2##NUM (TYPE2 *restrict a, TYPE *restrict b,         \
				  TYPE *restrict c)                            \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = (TYPE2) b[i] OP (TYPE2) c[i];                                     \
  }

#define DEF_OP_WWV(PREFIX, NUM, TYPE, TYPE2, OP)                               \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##_##TYPE2##NUM (TYPE2 *restrict a, TYPE2 *restrict b,        \
				  TYPE *restrict c)                            \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = b[i] OP (TYPE2) c[i];                                             \
  }

#define DEF_OP_WVV_SU(PREFIX, NUM, TYPE1, TYPE2, TYPE3, OP)                    \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##_##TYPE2##NUM (TYPE3 *restrict a, TYPE1 *restrict b,        \
				  TYPE2 *restrict c)                           \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = (TYPE3) b[i] OP (TYPE3) c[i];                                     \
  }

#define DEF_FMA_WVV(PREFIX, NUM, TYPE1, TYPE2)                                 \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE1##_##TYPE2##NUM (TYPE2 *restrict a, TYPE1 *restrict b,       \
				   TYPE1 *restrict c, TYPE2 *restrict d)       \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = (TYPE2) b[i] * (TYPE2) c[i] + d[i];                               \
  }

#define DEF_FMA_WVV_SU(PREFIX, NUM, TYPE1, TYPE2, TYPE3)                       \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE1##_##TYPE2##_##TYPE3##NUM (TYPE3 *restrict a,                \
					     TYPE1 *restrict b,                \
					     TYPE2 *restrict c,                \
					     TYPE3 *restrict d)                \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = (TYPE3) b[i] * (TYPE3) c[i] + d[i];                               \
  }

#define DEF_FNMA_WVV(PREFIX, NUM, TYPE1, TYPE2)                                \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE1##_##TYPE2##NUM (TYPE2 *restrict a, TYPE1 *restrict b,       \
				   TYPE1 *restrict c, TYPE2 *restrict d)       \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = d[i] - (TYPE2) b[i] * (TYPE2) c[i];                               \
  }

#define DEF_FMS_WVV(PREFIX, NUM, TYPE1, TYPE2)                                 \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE1##_##TYPE2##NUM (TYPE2 *restrict a, TYPE1 *restrict b,       \
				   TYPE1 *restrict c, TYPE2 *restrict d)       \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = (TYPE2) b[i] * (TYPE2) c[i] - d[i];                               \
  }

#define DEF_FNMS_WVV(PREFIX, NUM, TYPE1, TYPE2)                                \
  void __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE1##_##TYPE2##NUM (TYPE2 *restrict a, TYPE1 *restrict b,       \
				   TYPE1 *restrict c, TYPE2 *restrict d)       \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = -((TYPE2) b[i] * (TYPE2) c[i]) - d[i];                            \
  }

#define DEF_WIDEN_REDUC_PLUS(TYPE, TYPE2, NUM)                                 \
  TYPE2 __attribute__ ((noinline, noclone))                                    \
  reduc_plus_##TYPE##_##TYPE2##NUM (TYPE *__restrict a)                        \
  {                                                                            \
    TYPE2 r = 0;                                                               \
    for (int i = 0; i < NUM; ++i)                                              \
      r += a[i];                                                               \
    return r;                                                                  \
  }

#define DEF_NARROW_TRUNC_IMM(TYPE1, TYPE2, NUM)                                \
  void narrow_##TYPE1##_##TYPE2##_##NUM (TYPE1 *restrict a, TYPE2 *restrict b) \
  {                                                                            \
    for (int i = 0; i < NUM; i += 1)                                           \
      a[i] = (TYPE1) (b[i] >> 7);                                              \
  }

#define DEF_NARROW_TRUNC_XREG(TYPE1, TYPE2, NUM)                               \
  void narrow_##TYPE1##_##TYPE2##_##NUM (TYPE1 *restrict a, TYPE2 *restrict b, \
					 int shift)                            \
  {                                                                            \
    for (int i = 0; i < NUM; i += 1)                                           \
      a[i] = (TYPE1) (b[i] >> shift);                                          \
  }

#define DEF_NARROW_TRUNC_VREG(TYPE1, TYPE2, NUM)                               \
  void narrow_##TYPE1##_##TYPE2##_##NUM (TYPE1 *restrict a, TYPE2 *restrict b, \
					 int *restrict shift)                  \
  {                                                                            \
    for (int i = 0; i < NUM; i += 1)                                           \
      a[i] = (TYPE1) (b[i] >> shift[i]);                                       \
  }

#define DEF_COND_CONVERT(PREFIX, TYPE1, TYPE2, NUM)                            \
  __attribute__ ((noipa))                                                      \
  TYPE2 PREFIX##_##TYPE1##TYPE2##_##NUM (TYPE2 dst, TYPE1 a, int *cond)        \
  {                                                                            \
    for (int i = 0; i < NUM; i++)                                              \
      dst[i] = cond[i] ? a[i] : dst[i];                                        \
    return dst;                                                                \
  }

#define DEF_COND_FP_CONVERT(PREFIX, TYPE1, TYPE2, TYPE3, NUM)                  \
  __attribute__ ((noipa))                                                      \
  v##NUM##TYPE2 PREFIX##_##TYPE1##TYPE2##TYPE3##_##NUM (v##NUM##TYPE2 dst,     \
							v##NUM##TYPE1 a,       \
							int *cond)             \
  {                                                                            \
    for (int i = 0; i < NUM; i++)                                              \
      dst[i] = cond[i] ? (TYPE3) a[i] : dst[i];                                \
    return dst;                                                                \
  }

#define DEF_COND_CALL(PREFIX, NUM, TYPE, CALL)                                 \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i] = cond[i] ? CALL (a[i], b[i]) : c[i];                               \
    return v;                                                                  \
  }

#define DEF_COND_MULH(PREFIX, NUM, TYPE, TYPE2, TYPE3, SHIFT)                  \
  TYPE __attribute__ ((noinline, noclone))                                     \
  PREFIX##_##TYPE##NUM (TYPE a, TYPE b, TYPE c, TYPE cond)                     \
  {                                                                            \
    TYPE v;                                                                    \
    for (int i = 0; i < NUM; ++i)                                              \
      v[i]                                                                     \
	= cond[i] ? (TYPE3) (((TYPE2) a[i] * (TYPE2) b[i]) >> SHIFT) : c[i];   \
    return v;                                                                  \
  }

#define DEF_COND_OP_WVV(PREFIX, NUM, TYPE, TYPE2, TYPE3, OP)                   \
  v##NUM##TYPE2 __attribute__ ((noinline, noclone))                            \
  PREFIX##_##TYPE##_##TYPE2##NUM (v##NUM##TYPE2 a, v##NUM##TYPE b,             \
				  v##NUM##TYPE c, int *restrict cond)          \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = cond[i] ? (TYPE3) b[i] OP (TYPE3) c[i] : a[i];                    \
    return a;                                                                  \
  }

#define DEF_COND_OP_WVV_SU(PREFIX, NUM, TYPE, TYPE2, TYPE3, OP)                \
  v##NUM##TYPE2 __attribute__ ((noinline, noclone))                            \
  PREFIX##_##TYPE##_##TYPE2##NUM (v##NUM##TYPE2 a, v##NUM##u##TYPE b,          \
				  v##NUM##TYPE c, int *restrict cond)          \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = cond[i] ? (TYPE3) b[i] OP (TYPE3) c[i] : a[i];                    \
    return a;                                                                  \
  }

#define DEF_COND_OP_WWV(PREFIX, NUM, TYPE, TYPE2, TYPE3, OP)                   \
  v##NUM##TYPE2 __attribute__ ((noinline, noclone))                            \
  PREFIX##_##TYPE##_##TYPE2##NUM (v##NUM##TYPE2 a, v##NUM##TYPE2 b,            \
				  v##NUM##TYPE c, int *restrict cond)          \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = cond[i] ? (TYPE3) b[i] OP (TYPE3) c[i] : a[i];                    \
    return a;                                                                  \
  }

#define DEF_WFMA_VV(PREFIX, NUM, TYPE, TYPE2, TYPE3)                           \
  v##NUM##TYPE2 __attribute__ ((noinline, noclone))                            \
  PREFIX##_##TYPE##TYPE2##TYPE3##NUM (v##NUM##TYPE2 a, v##NUM##TYPE b,         \
				      v##NUM##TYPE c, int *restrict cond)      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = cond[i] ? (TYPE3) b[i] * (TYPE3) c[i] + a[i] : a[i];              \
    return a;                                                                  \
  }

#define DEF_WFNMA_VV(PREFIX, NUM, TYPE, TYPE2, TYPE3)                          \
  v##NUM##TYPE2 __attribute__ ((noinline, noclone))                            \
  PREFIX##_##TYPE##TYPE2##TYPE3##NUM (v##NUM##TYPE2 a, v##NUM##TYPE b,         \
				      v##NUM##TYPE c, int *restrict cond)      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = cond[i] ? a[i] - (TYPE3) b[i] * (TYPE3) c[i] : a[i];              \
    return a;                                                                  \
  }

#define DEF_WFMS_VV(PREFIX, NUM, TYPE, TYPE2, TYPE3)                           \
  v##NUM##TYPE2 __attribute__ ((noinline, noclone))                            \
  PREFIX##_##TYPE##TYPE2##TYPE3##NUM (v##NUM##TYPE2 a, v##NUM##TYPE b,         \
				      v##NUM##TYPE c, int *restrict cond)      \
  {                                                                            \
    for (int i = 0; i < NUM; ++i)                                              \
      a[i] = cond[i] ? (TYPE3) b[i] * (TYPE3) c[i] - a[i] : a[i];              \
    return a;                                                                  \
  }

#define DEF_COND_NARROW_TRUNC_IMM(TYPE1, TYPE2, TYPE3, NUM)                    \
  v##NUM##TYPE1 narrow_##TYPE1##_##TYPE2##_##NUM (v##NUM##TYPE1 a,             \
						  v##NUM##TYPE2 b,             \
						  int *__restrict cond)        \
  {                                                                            \
    for (int i = 0; i < NUM; i += 1)                                           \
      a[i] = cond[i] ? (TYPE3) (b[i] >> 7) : a[i];                             \
    return a;                                                                  \
  }

#define DEF_COND_NARROW_TRUNC_XREG(TYPE1, TYPE2, TYPE3, NUM)                   \
  v##NUM##TYPE1 narrow_##TYPE1##_##TYPE2##_##NUM (v##NUM##TYPE1 a,             \
						  v##NUM##TYPE2 b, int shift,  \
						  int *__restrict cond)        \
  {                                                                            \
    for (int i = 0; i < NUM; i += 1)                                           \
      a[i] = cond[i] ? (TYPE3) (b[i] >> shift) : a[i];                         \
    return a;                                                                  \
  }

#define DEF_CONSECUTIVE(TYPE, NUM)                                             \
  TYPE f##TYPE (TYPE a, TYPE b)                                                \
  {                                                                            \
    return __builtin_shufflevector (a, b, MASK_##NUM);                         \
  }

#define DEF_COMBINE(TYPE1, TYPE2, NUM, ...)                                    \
  void combine_##TYPE1##_##TYPE2##_##NUM (TYPE2 *out, TYPE2 x, TYPE2 y)        \
  {                                                                            \
    v##NUM##TYPE1 v = {__VA_ARGS__};                                           \
    *(v##NUM##TYPE1 *) out = v;                                                \
  }

#define DEF_TRAILING(TYPE1, TYPE2, NUM, ...)                                   \
  void init_##TYPE1##_##TYPE2##_##NUM (TYPE2 var0, TYPE2 var1, TYPE2 var2,     \
				       TYPE2 var3, TYPE2 *__restrict out)      \
  {                                                                            \
    TYPE1 v = {__VA_ARGS__};                                                   \
    *(TYPE1 *) out = v;                                                        \
  }

#define DEF_RET1_ARG0(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG0 ()                                                          \
  {                                                                            \
    TYPE r = {};                                                               \
    return r;                                                                  \
  }

#define DEF_RET1_ARG1(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG1 (TYPE a1)                                                   \
  {                                                                            \
    return a1;                                                                 \
  }

#define DEF_RET1_ARG2(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG2 (TYPE a1, TYPE a2)                                          \
  {                                                                            \
    return a1 + a2;                                                            \
  }

#define DEF_RET1_ARG3(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG3 (TYPE a1, TYPE a2, TYPE a3)                                 \
  {                                                                            \
    return a1 + a2 + a3;                                                       \
  }

#define DEF_RET1_ARG4(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG4 (TYPE a1, TYPE a2, TYPE a3, TYPE a4)                        \
  {                                                                            \
    return a1 + a2 + a3 + a4;                                                  \
  }

#define DEF_RET1_ARG5(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG5 (TYPE a1, TYPE a2, TYPE a3, TYPE a4, TYPE a5)               \
  {                                                                            \
    return a1 + a2 + a3 + a4 + a5;                                             \
  }

#define DEF_RET1_ARG6(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG6 (TYPE a1, TYPE a2, TYPE a3, TYPE a4, TYPE a5, TYPE a6)      \
  {                                                                            \
    return a1 + a2 + a3 + a4 + a5 + a6;                                        \
  }

#define DEF_RET1_ARG7(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG7 (TYPE a1, TYPE a2, TYPE a3, TYPE a4, TYPE a5, TYPE a6,      \
		    TYPE a7)                                                   \
  {                                                                            \
    return a1 + a2 + a3 + a4 + a5 + a6 + a7;                                   \
  }

#define DEF_RET1_ARG8(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG8 (TYPE a1, TYPE a2, TYPE a3, TYPE a4, TYPE a5, TYPE a6,      \
		    TYPE a7, TYPE a8)                                          \
  {                                                                            \
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;                              \
  }

#define DEF_RET1_ARG9(TYPE)                                                    \
  TYPE __attribute__((noinline))                                               \
  TYPE##_RET1_ARG9 (TYPE a1, TYPE a2, TYPE a3, TYPE a4, TYPE a5, TYPE a6,      \
		    TYPE a7, TYPE a8, TYPE a9)                                 \
  {                                                                            \
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;                         \
  }
