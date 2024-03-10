/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target size32plus } */

typedef signed char int8_T;
typedef int int32_T;

typedef struct {
  int8_T a;
} struct0_T;

typedef struct {
  struct0_T f10[4];
} struct_T;

typedef struct {
  struct_T f9[4];
} b_struct_T;

typedef struct {
  b_struct_T f8[4];
} c_struct_T;

typedef struct {
  c_struct_T f7[4];
} d_struct_T;

typedef struct {
  d_struct_T f6[4];
} e_struct_T;

typedef struct {
  e_struct_T f5[4];
} f_struct_T;

typedef struct {
  f_struct_T f4[4];
} g_struct_T;

typedef struct {
  g_struct_T f3[4];
} h_struct_T;

typedef struct {
  h_struct_T f2[4];
} i_struct_T;

typedef struct {
  i_struct_T f1[4];
} j_struct_T;

typedef struct {
  struct {
    j_struct_T ds21[4];
    i_struct_T ds20[4];
    i_struct_T r9;
  } f0;
} deep_struct_arraysStackData;

/* Function Definitions */
void deep_struct_arrays(deep_struct_arraysStackData *SD,
  int8_T in1, int8_T inCount, int8_T *out1, int8_T *out2, struct0_T out3[4])
{
  struct0_T r;
  struct_T r1;
  b_struct_T r2;
  c_struct_T r3;
  d_struct_T r4;
  e_struct_T r5;
  f_struct_T r6;
  g_struct_T r7;
  h_struct_T r8;
  int32_T count;
  int32_T i;

  /*  Check properties of input in1 */
  /*  Check properties of input inCount */
  /*  Copyright 2006 The MathWorks, Inc. */
  r.a = in1;
  r1.f10[0] = r;
  r1.f10[1] = r;
  r1.f10[2] = r;
  r1.f10[3] = r;
  r2.f9[0] = r1;
  r2.f9[1] = r1;
  r2.f9[2] = r1;
  r2.f9[3] = r1;
  r3.f8[0] = r2;
  r3.f8[1] = r2;
  r3.f8[2] = r2;
  r3.f8[3] = r2;
  r4.f7[0] = r3;
  r4.f7[1] = r3;
  r4.f7[2] = r3;
  r4.f7[3] = r3;
  r5.f6[0] = r4;
  r5.f6[1] = r4;
  r5.f6[2] = r4;
  r5.f6[3] = r4;
  r6.f5[0] = r5;
  r6.f5[1] = r5;
  r6.f5[2] = r5;
  r6.f5[3] = r5;
  r7.f4[0] = r6;
  r7.f4[1] = r6;
  r7.f4[2] = r6;
  r7.f4[3] = r6;
  r8.f3[0] = r7;
  r8.f3[1] = r7;
  r8.f3[2] = r7;
  r8.f3[3] = r7;
  SD->f0.r9.f2[0] = r8;
  SD->f0.r9.f2[1] = r8;
  SD->f0.r9.f2[2] = r8;
  SD->f0.r9.f2[3] = r8;
  SD->f0.ds20[0] = SD->f0.r9;
  SD->f0.ds20[3] = SD->f0.r9;
  count = 0;
  while (count < inCount) {
    i = in1 + SD->f0.ds20[0].f2[0].f3[0].f4[0].f5[0].f6[0].f7[0].f8[0].f9[0]
      .f10[0].a;
    if (i > 127) {
      i = 127;
    } else {
      if (i < -128) {
        i = -128;
      }
    }

    SD->f0.ds20[0].f2[0].f3[0].f4[0].f5[0].f6[0].f7[0].f8[0].f9[0].f10[0].a =
      (int8_T)i;
    i = SD->f0.ds20[3].f2[3].f3[3].f4[3].f5[3].f6[3].f7[3].f8[3].f9[3].f10[3].a
      + 3;
    if (i > 127) {
      i = 127;
    }

    SD->f0.ds20[3].f2[3].f3[3].f4[3].f5[3].f6[3].f7[3].f8[3].f9[3].f10[3].a =
      (int8_T)i;
    count++;
  }

  if (inCount > 10) {
    SD->f0.ds21[0].f1[1].f2[2].f3[3].f4[3].f5[3].f6[3].f7[3].f8[3].f9[3].f10[3].
      a = 14;
  } else {
    SD->f0.ds21[0].f1[1].f2[2].f3[3].f4[3].f5[3].f6[3].f7[3].f8[3].f9[3].f10[3].
      a = 16;
  }

  *out1 = SD->f0.ds20[0].f2[0].f3[0].f4[0].f5[0].f6[0].f7[0].f8[0].f9[0].f10[0].
    a;
  *out2 = SD->f0.ds20[3].f2[3].f3[3].f4[3].f5[3].f6[3].f7[3].f8[3].f9[3].f10[3].
    a;
  out3[0] = r;
  out3[1] = r;
  out3[2] = r;
  out3[3] = SD->f0.ds21[0].f1[1].f2[2].f3[3].f4[3].f5[3].f6[3].f7[3].f8[3].f9[3]
    .f10[3];
}
