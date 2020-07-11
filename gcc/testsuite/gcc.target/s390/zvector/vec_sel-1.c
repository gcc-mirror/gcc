/* { dg-do run } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector --save-temps -Wno-attributes" } */

#include <string.h>
#include <vecintrin.h>

typedef vector signed char v16qi;
typedef vector unsigned char uv16qi;
typedef vector bool char bv16qi;

typedef vector signed short int v8hi;
typedef vector unsigned short int uv8hi;
typedef vector bool short int bv8hi;

typedef vector signed int v4si;
typedef vector unsigned int uv4si;
typedef vector bool int bv4si;

typedef vector signed long long v2di;
typedef vector unsigned long long uv2di;
typedef vector bool long long bv2di;

typedef vector float v4sf;
typedef vector double v2df;

#define NUM_CONSTS 8

const v16qi v16qi_vals[NUM_CONSTS] =
  { (v16qi){ 1 },
    (v16qi){ 2 },
    (v16qi){ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 },
    (v16qi){ 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2 },
    (v16qi){ -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 },
    (v16qi){ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },
    (v16qi){ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 },
    (v16qi){ 15,14,13,12,11,10,9,8,7,6,5,4,3,2,1 }
  };

const v8hi v8hi_vals[NUM_CONSTS] =
  { (v8hi){ 1 },
    (v8hi){ 2 },
    (v8hi){ 1,1,1,1,1,1,1,1 },
    (v8hi){ 2,2,2,2,2,2,2,2 },
    (v8hi){ -1,-1,-1,-1,-1,-1,-1,-1 },
    (v8hi){ 0,0,0,0,0,0,0,0 },
    (v8hi){ 1,2,3,4,5,6,7,8 },
    (v8hi){ 8,7,6,5,4,3,2,1 }
  };

const v4si v4si_vals[NUM_CONSTS] =
  { (v4si){ 1 },
    (v4si){ 2 },
    (v4si){ 1,1,1,1 },
    (v4si){ 2,2,2,2 },
    (v4si){ -1,-1,-1,-1 },
    (v4si){ 0,0,0,0 },
    (v4si){ 1,2,3,4 },
    (v4si){ 4,3,2,1 }
  };

const v2di v2di_vals[NUM_CONSTS] =
  { (v2di){ 1 },
    (v2di){ 2 },
    (v2di){ 1,1 },
    (v2di){ 2,2 },
    (v2di){ -1,-1 },
    (v2di){ 0,0 },
    (v2di){ 1,2 },
    (v2di){ 2,1 }
  };

const v4sf v4sf_vals[NUM_CONSTS] =
  { (v4sf){ 1.0f },
    (v4sf){ 2.0f },
    (v4sf){ 1.0f,1.0f,1.0f,1.0f },
    (v4sf){ 2.0f,2.0f,2.0f,2.0f },
    (v4sf){ -1.0f,-1.0f,-1.0f,-1.0f },
    (v4sf){ 0.0f,0.0f,0.0f,0.0f },
    (v4sf){ 1.1f,2.1f,3.1f,4.1f },
    (v4sf){ 4.1f,3.1f,2.1f,1.1f }
  };

const v2df v2df_vals[NUM_CONSTS] =
  { (v2df){ 1.0 },
    (v2df){ 2.0 },
    (v2df){ 1.0,1.0 },
    (v2df){ 2.0,2.0 },
    (v2df){ -1.0,-1.0 },
    (v2df){ 0.0,0.0 },
    (v2df){ 1.1,2.1 },
    (v2df){ 2.1,1.1 }
  };

/* Each bit of the result vector has the value of the corresponding
   bit of A if the corresponding bit of C is 0, or the value of the
   corresponding bit of B otherwise.  */
void __attribute__((noinline, noclone, target ("arch=zEC12")))
emul (unsigned char *result, unsigned char *a,
      unsigned char *b, unsigned char *c)
{
  for (int i = 0; i < 16; i++)
    result[i] = (a[i] & ~c[i]) | (b[i] & c[i]);
}

#define GENFUNC(NAME, T1, T2)						\
  T1 __attribute__((noinline, noclone))					\
  NAME##_reg (T1 a, T1 b, T2 c) { return vec_sel (a, b, c); }		\
  void __attribute__((noinline, noclone))				\
  NAME##_mem (T1 *a, T1 *b, T2 *c, T1 *out) { *out = vec_sel (*a, *b, *c); } \
  T1 __attribute__((always_inline))					\
  NAME##_const (T1 a, T1 b, T2 c) { return vec_sel (a, b, c); }

GENFUNC (vec_sel_b8_a, bv16qi, uv16qi)
GENFUNC (vec_sel_b8_b, bv16qi, bv16qi)
GENFUNC (vec_sel_s8_a,  v16qi, uv16qi)
GENFUNC (vec_sel_s8_b,  v16qi, bv16qi)
GENFUNC (vec_sel_u8_a, uv16qi, uv16qi)
GENFUNC (vec_sel_u8_b, uv16qi, bv16qi)

GENFUNC (vec_sel_b16_a, bv8hi, uv8hi)
GENFUNC (vec_sel_b16_b, bv8hi, bv8hi)
GENFUNC (vec_sel_s16_a,  v8hi, uv8hi)
GENFUNC (vec_sel_s16_b,  v8hi, bv8hi)
GENFUNC (vec_sel_u16_a, uv8hi, uv8hi)
GENFUNC (vec_sel_u16_b, uv8hi, bv8hi)

GENFUNC (vec_sel_b32_a, bv4si, uv4si)
GENFUNC (vec_sel_b32_b, bv4si, bv4si)
GENFUNC (vec_sel_s32_a,  v4si, uv4si)
GENFUNC (vec_sel_s32_b,  v4si, bv4si)
GENFUNC (vec_sel_u32_a, uv4si, uv4si)
GENFUNC (vec_sel_u32_b, uv4si, bv4si)

GENFUNC (vec_sel_b64_a, bv2di, uv2di)
GENFUNC (vec_sel_b64_b, bv2di, bv2di)
GENFUNC (vec_sel_s64_a,  v2di, uv2di)
GENFUNC (vec_sel_s64_b,  v2di, bv2di)
GENFUNC (vec_sel_u64_a, uv2di, uv2di)
GENFUNC (vec_sel_u64_b, uv2di, bv2di)

GENFUNC (vec_sel_flt_a,  v4sf, uv4si)
GENFUNC (vec_sel_flt_b,  v4sf, bv4si)

GENFUNC (vec_sel_dbl_a,  v2df, uv2di)
GENFUNC (vec_sel_dbl_b,  v2df, bv2di)

#define TESTFUNC(NAME, T1, T2, VAL_TYPE)				\
  for (int i = 0; i < NUM_CONSTS; i++)					\
    for (int j = 0; j < NUM_CONSTS; j++)				\
      for (int k = 0; k < NUM_CONSTS; k++)				\
	{								\
	  unsigned char result[16];					\
	  T1 in1 = (T1)VAL_TYPE##_vals[i];				\
	  T1 in2 = (T1)VAL_TYPE##_vals[j];				\
	  T2 in3 = (T2)VAL_TYPE##_vals[k];				\
	  emul (result, (char*)&in1, (char*)&in2, (char*)&in3);		\
									\
	  T1 reg = NAME##_reg (in1, in2, in3);				\
	  if (memcmp ((char*)&reg, result, 16) != 0)			\
	    __builtin_abort ();						\
									\
	  T1 mem;							\
	  NAME##_mem (&in1, &in2, &in3, &mem);				\
	  if (memcmp ((char*)&mem, result, 16) != 0)			\
	    __builtin_abort ();						\
									\
	  T1 cons = NAME##_const (in1, in2, in3);			\
	  if (memcmp ((char*)&cons, result, 16) != 0)			\
	    __builtin_abort ();						\
	}

int
main ()
{
  TESTFUNC (vec_sel_b8_a, bv16qi, uv16qi, v16qi);
  TESTFUNC (vec_sel_b8_b, bv16qi, bv16qi, v16qi);
  TESTFUNC (vec_sel_s8_a,  v16qi, uv16qi, v16qi);
  TESTFUNC (vec_sel_s8_b,  v16qi, bv16qi, v16qi);
  TESTFUNC (vec_sel_u8_a, uv16qi, uv16qi, v16qi);
  TESTFUNC (vec_sel_u8_b, uv16qi, bv16qi, v16qi);

  TESTFUNC (vec_sel_b16_a, bv8hi, uv8hi, v8hi);
  TESTFUNC (vec_sel_b16_b, bv8hi, bv8hi, v8hi);
  TESTFUNC (vec_sel_s16_a,  v8hi, uv8hi, v8hi);
  TESTFUNC (vec_sel_s16_b,  v8hi, bv8hi, v8hi);
  TESTFUNC (vec_sel_u16_a, uv8hi, uv8hi, v8hi);
  TESTFUNC (vec_sel_u16_b, uv8hi, bv8hi, v8hi);

  TESTFUNC (vec_sel_b32_a, bv4si, uv4si, v4si);
  TESTFUNC (vec_sel_b32_b, bv4si, bv4si, v4si);
  TESTFUNC (vec_sel_s32_a,  v4si, uv4si, v4si);
  TESTFUNC (vec_sel_s32_b,  v4si, bv4si, v4si);
  TESTFUNC (vec_sel_u32_a, uv4si, uv4si, v4si);
  TESTFUNC (vec_sel_u32_b, uv4si, bv4si, v4si);

  TESTFUNC (vec_sel_b64_a, bv2di, uv2di, v2di);
  TESTFUNC (vec_sel_b64_b, bv2di, bv2di, v2di);
  TESTFUNC (vec_sel_s64_a,  v2di, uv2di, v2di);
  TESTFUNC (vec_sel_s64_b,  v2di, bv2di, v2di);
  TESTFUNC (vec_sel_u64_a, uv2di, uv2di, v2di);
  TESTFUNC (vec_sel_u64_b, uv2di, bv2di, v2di);

  TESTFUNC (vec_sel_flt_a,  v4sf, uv4si, v4sf);
  TESTFUNC (vec_sel_flt_b,  v4sf, bv4si, v4sf);

  TESTFUNC (vec_sel_dbl_a,  v2df, uv2di, v2df);
  TESTFUNC (vec_sel_dbl_b,  v2df, bv2di, v2df);
}

/* { dg-final { scan-assembler {\n\tvsel\t} } } */
