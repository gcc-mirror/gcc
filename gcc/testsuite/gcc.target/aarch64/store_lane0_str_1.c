/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int v2si __attribute__ ((vector_size (8)));
typedef float v2sf __attribute__ ((vector_size (8)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef __fp16 v4hf __attribute__ ((vector_size (8)));
typedef char v8qi __attribute__ ((vector_size (8)));

typedef int v4si __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef __fp16 v8hf __attribute__ ((vector_size (16)));
typedef char v16qi __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define LANE(N) (N - 1)
#else
#define LANE(N) 0
#endif

#define FUNC(T, E, N)			\
void					\
store_lane_##T (T x, E *y)		\
{					\
  y[0] = x[N - 1 - LANE (N)];		\
  y[3] = x[LANE (N)];			\
}

FUNC (v2si, int, 2)
FUNC (v2sf, float, 2)
FUNC (v4hi, short, 4)
FUNC (v4hf, __fp16, 4)
FUNC (v8qi, char, 8)

FUNC (v4si, int, 4)
FUNC (v4sf, float, 4)
FUNC (v8hi, short, 8)
FUNC (v8hf, __fp16, 8)
FUNC (v16qi, char, 16)
FUNC (v2di, long long, 2)
FUNC (v2df, double, 2)

/* When storing lane zero of a vector we can use the scalar STR instruction
   that supports more addressing modes.  */

/* { dg-final { scan-assembler-times "str\ts\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "str\tb\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "str\th\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "str\td\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-not "umov" } } */
/* { dg-final { scan-assembler-not "dup" } } */
