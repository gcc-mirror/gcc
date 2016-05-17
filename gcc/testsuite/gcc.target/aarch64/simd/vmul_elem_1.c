/* Test the vmul_n_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

#include "arm_neon.h"

extern void abort (void);

#define A (132.4f)
#define B (-0.0f)
#define C (-34.8f)
#define D (289.34f)
float32_t expected2_1[2] = {A * A, B * A};
float32_t expected2_2[2] = {A * B, B * B};
float32_t expected4_1[4] = {A * A, B * A, C * A, D * A};
float32_t expected4_2[4] = {A * B, B * B, C * B, D * B};
float32_t expected4_3[4] = {A * C, B * C, C * C, D * C};
float32_t expected4_4[4] = {A * D, B * D, C * D, D * D};
float32_t _elemA = A;
float32_t _elemB = B;
float32_t _elemC = C;
float32_t _elemD = D;

#define AD (1234.5)
#define BD (-0.0)
#define CD (71.3)
#define DD (-1024.4)
float64_t expectedd2_1[2] = {AD * CD, BD * CD};
float64_t expectedd2_2[2] = {AD * DD, BD * DD};
float64_t _elemdC = CD;
float64_t _elemdD = DD;


#define AS (1024)
#define BS (-31)
#define CS (0)
#define DS (655)
int32_t expecteds2_1[2] = {AS * AS, BS * AS};
int32_t expecteds2_2[2] = {AS * BS, BS * BS};
int32_t expecteds4_1[4] = {AS * AS, BS * AS, CS * AS, DS * AS};
int32_t expecteds4_2[4] = {AS * BS, BS * BS, CS * BS, DS * BS};
int32_t expecteds4_3[4] = {AS * CS, BS * CS, CS * CS, DS * CS};
int32_t expecteds4_4[4] = {AS * DS, BS * DS, CS * DS, DS * DS};
int32_t _elemsA = AS;
int32_t _elemsB = BS;
int32_t _elemsC = CS;
int32_t _elemsD = DS;

#define AH ((int16_t) 0)
#define BH ((int16_t) -32)
#define CH ((int16_t) 102)
#define DH ((int16_t) -51)
#define EH ((int16_t) 71)
#define FH ((int16_t) -91)
#define GH ((int16_t) 48)
#define HH ((int16_t) 255)
int16_t expectedh4_1[4] = {AH * AH, BH * AH, CH * AH, DH * AH};
int16_t expectedh4_2[4] = {AH * BH, BH * BH, CH * BH, DH * BH};
int16_t expectedh4_3[4] = {AH * CH, BH * CH, CH * CH, DH * CH};
int16_t expectedh4_4[4] = {AH * DH, BH * DH, CH * DH, DH * DH};
int16_t expectedh8_1[8] = {AH * AH, BH * AH, CH * AH, DH * AH,
			   EH * AH, FH * AH, GH * AH, HH * AH};
int16_t expectedh8_2[8] = {AH * BH, BH * BH, CH * BH, DH * BH,
			   EH * BH, FH * BH, GH * BH, HH * BH};
int16_t expectedh8_3[8] = {AH * CH, BH * CH, CH * CH, DH * CH,
			   EH * CH, FH * CH, GH * CH, HH * CH};
int16_t expectedh8_4[8] = {AH * DH, BH * DH, CH * DH, DH * DH,
			   EH * DH, FH * DH, GH * DH, HH * DH};
int16_t expectedh8_5[8] = {AH * EH, BH * EH, CH * EH, DH * EH,
			   EH * EH, FH * EH, GH * EH, HH * EH};
int16_t expectedh8_6[8] = {AH * FH, BH * FH, CH * FH, DH * FH,
			   EH * FH, FH * FH, GH * FH, HH * FH};
int16_t expectedh8_7[8] = {AH * GH, BH * GH, CH * GH, DH * GH,
			   EH * GH, FH * GH, GH * GH, HH * GH};
int16_t expectedh8_8[8] = {AH * HH, BH * HH, CH * HH, DH * HH,
			   EH * HH, FH * HH, GH * HH, HH * HH};
int16_t _elemhA = AH;
int16_t _elemhB = BH;
int16_t _elemhC = CH;
int16_t _elemhD = DH;
int16_t _elemhE = EH;
int16_t _elemhF = FH;
int16_t _elemhG = GH;
int16_t _elemhH = HH;

#define AUS (1024)
#define BUS (31)
#define CUS (0)
#define DUS (655)
uint32_t expectedus2_1[2] = {AUS * AUS, BUS * AUS};
uint32_t expectedus2_2[2] = {AUS * BUS, BUS * BUS};
uint32_t expectedus4_1[4] = {AUS * AUS, BUS * AUS, CUS * AUS, DUS * AUS};
uint32_t expectedus4_2[4] = {AUS * BUS, BUS * BUS, CUS * BUS, DUS * BUS};
uint32_t expectedus4_3[4] = {AUS * CUS, BUS * CUS, CUS * CUS, DUS * CUS};
uint32_t expectedus4_4[4] = {AUS * DUS, BUS * DUS, CUS * DUS, DUS * DUS};
uint32_t _elemusA = AUS;
uint32_t _elemusB = BUS;
uint32_t _elemusC = CUS;
uint32_t _elemusD = DUS;

#define AUH ((uint16_t) 0)
#define BUH ((uint16_t) 32)
#define CUH ((uint16_t) 102)
#define DUH ((uint16_t) 51)
#define EUH ((uint16_t) 71)
#define FUH ((uint16_t) 91)
#define GUH ((uint16_t) 48)
#define HUH ((uint16_t) 255)
uint16_t expecteduh4_1[4] = {AUH * AUH, BUH * AUH, CUH * AUH, DUH * AUH};
uint16_t expecteduh4_2[4] = {AUH * BUH, BUH * BUH, CUH * BUH, DUH * BUH};
uint16_t expecteduh4_3[4] = {AUH * CUH, BUH * CUH, CUH * CUH, DUH * CUH};
uint16_t expecteduh4_4[4] = {AUH * DUH, BUH * DUH, CUH * DUH, DUH * DUH};
uint16_t expecteduh8_1[8] = {AUH * AUH, BUH * AUH, CUH * AUH, DUH * AUH,
			     EUH * AUH, FUH * AUH, GUH * AUH, HUH * AUH};
uint16_t expecteduh8_2[8] = {AUH * BUH, BUH * BUH, CUH * BUH, DUH * BUH,
			     EUH * BUH, FUH * BUH, GUH * BUH, HUH * BUH};
uint16_t expecteduh8_3[8] = {AUH * CUH, BUH * CUH, CUH * CUH, DUH * CUH,
			     EUH * CUH, FUH * CUH, GUH * CUH, HUH * CUH};
uint16_t expecteduh8_4[8] = {AUH * DUH, BUH * DUH, CUH * DUH, DUH * DUH,
			     EUH * DUH, FUH * DUH, GUH * DUH, HUH * DUH};
uint16_t expecteduh8_5[8] = {AUH * EUH, BUH * EUH, CUH * EUH, DUH * EUH,
			     EUH * EUH, FUH * EUH, GUH * EUH, HUH * EUH};
uint16_t expecteduh8_6[8] = {AUH * FUH, BUH * FUH, CUH * FUH, DUH * FUH,
			     EUH * FUH, FUH * FUH, GUH * FUH, HUH * FUH};
uint16_t expecteduh8_7[8] = {AUH * GUH, BUH * GUH, CUH * GUH, DUH * GUH,
			     EUH * GUH, FUH * GUH, GUH * GUH, HUH * GUH};
uint16_t expecteduh8_8[8] = {AUH * HUH, BUH * HUH, CUH * HUH, DUH * HUH,
			     EUH * HUH, FUH * HUH, GUH * HUH, HUH * HUH};
uint16_t _elemuhA = AUH;
uint16_t _elemuhB = BUH;
uint16_t _elemuhC = CUH;
uint16_t _elemuhD = DUH;
uint16_t _elemuhE = EUH;
uint16_t _elemuhF = FUH;
uint16_t _elemuhG = GUH;
uint16_t _elemuhH = HUH;

void
check_v2sf (float32_t elemA, float32_t elemB)
{
  int32_t indx;
  const float32_t vec32x2_buf[2] = {A, B};
  float32x2_t vec32x2_src = vld1_f32 (vec32x2_buf);
  float32x2_t vec32x2_res = vec32x2_src * elemA;

  for (indx = 0; indx < 2; indx++)
    if (* (uint32_t *) &vec32x2_res[indx] != * (uint32_t *) &expected2_1[indx])
      abort ();

  vec32x2_res = vec32x2_src * elemB;

  for (indx = 0; indx < 2; indx++)
    if (* (uint32_t *) &vec32x2_res[indx] != * (uint32_t *) &expected2_2[indx])
      abort ();

/* { dg-final { scan-assembler-times "fmul\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.s\\\[0\\\]" 2 } } */
}

void
check_v4sf (float32_t elemA, float32_t elemB, float32_t elemC, float32_t elemD)
{
  int32_t indx;
  const float32_t vec32x4_buf[4] = {A, B, C, D};
  float32x4_t vec32x4_src = vld1q_f32 (vec32x4_buf);
  float32x4_t vec32x4_res = vec32x4_src * elemA;

  for (indx = 0; indx < 4; indx++)
    if (* (uint32_t *) &vec32x4_res[indx] != * (uint32_t *) &expected4_1[indx])
      abort ();

  vec32x4_res = vec32x4_src * elemB;

  for (indx = 0; indx < 4; indx++)
    if (* (uint32_t *) &vec32x4_res[indx] != * (uint32_t *) &expected4_2[indx])
      abort ();

  vec32x4_res = vec32x4_src * elemC;

  for (indx = 0; indx < 4; indx++)
    if (* (uint32_t *) &vec32x4_res[indx] != * (uint32_t *) &expected4_3[indx])
      abort ();

  vec32x4_res = vec32x4_src * elemD;

  for (indx = 0; indx < 4; indx++)
    if (* (uint32_t *) &vec32x4_res[indx] != * (uint32_t *) &expected4_4[indx])
      abort ();

/* { dg-final { scan-assembler-times "fmul\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.s\\\[0\\\]" 4 } } */
}

void
check_v2df (float64_t elemdC, float64_t elemdD)
{
  int32_t indx;
  const float64_t vec64x2_buf[2] = {AD, BD};
  float64x2_t vec64x2_src = vld1q_f64 (vec64x2_buf);
  float64x2_t vec64x2_res = vec64x2_src * elemdC;

  for (indx = 0; indx < 2; indx++)
    if (* (uint64_t *) &vec64x2_res[indx] != * (uint64_t *) &expectedd2_1[indx])
      abort ();

  vec64x2_res = vec64x2_src * elemdD;

  for (indx = 0; indx < 2; indx++)
    if (* (uint64_t *) &vec64x2_res[indx] != * (uint64_t *) &expectedd2_2[indx])
      abort ();

/* { dg-final { scan-assembler-times "fmul\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.d\\\[0\\\]" 2 } } */
}

void
check_v2si (int32_t elemsA, int32_t elemsB)
{
  int32_t indx;
  const int32_t vecs32x2_buf[2] = {AS, BS};
  int32x2_t vecs32x2_src = vld1_s32 (vecs32x2_buf);
  int32x2_t vecs32x2_res = vecs32x2_src * elemsA;

  for (indx = 0; indx < 2; indx++)
    if (vecs32x2_res[indx] != expecteds2_1[indx])
      abort ();

  vecs32x2_res = vecs32x2_src * elemsB;

  for (indx = 0; indx < 2; indx++)
    if (vecs32x2_res[indx] != expecteds2_2[indx])
      abort ();
}

void
check_v2si_unsigned (uint32_t elemusA, uint32_t elemusB)
{
  int indx;
  const uint32_t vecus32x2_buf[2] = {AUS, BUS};
  uint32x2_t vecus32x2_src = vld1_u32 (vecus32x2_buf);
  uint32x2_t vecus32x2_res = vecus32x2_src * elemusA;

  for (indx = 0; indx < 2; indx++)
    if (vecus32x2_res[indx] != expectedus2_1[indx])
      abort ();

  vecus32x2_res = vecus32x2_src * elemusB;

  for (indx = 0; indx < 2; indx++)
    if (vecus32x2_res[indx] != expectedus2_2[indx])
      abort ();

/* { dg-final { scan-assembler-times "\tmul\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.s\\\[0\\\]" 4 } } */
}

void
check_v4si (int32_t elemsA, int32_t elemsB, int32_t elemsC, int32_t elemsD)
{
  int32_t indx;
  const int32_t vecs32x4_buf[4] = {AS, BS, CS, DS};
  int32x4_t vecs32x4_src = vld1q_s32 (vecs32x4_buf);
  int32x4_t vecs32x4_res = vecs32x4_src * elemsA;

  for (indx = 0; indx < 4; indx++)
    if (vecs32x4_res[indx] != expecteds4_1[indx])
      abort ();

  vecs32x4_res = vecs32x4_src * elemsB;

  for (indx = 0; indx < 4; indx++)
    if (vecs32x4_res[indx] != expecteds4_2[indx])
      abort ();

  vecs32x4_res = vecs32x4_src * elemsC;

  for (indx = 0; indx < 4; indx++)
    if (vecs32x4_res[indx] != expecteds4_3[indx])
      abort ();

  vecs32x4_res = vecs32x4_src * elemsD;

  for (indx = 0; indx < 4; indx++)
    if (vecs32x4_res[indx] != expecteds4_4[indx])
      abort ();
}

void
check_v4si_unsigned (uint32_t elemusA, uint32_t elemusB, uint32_t elemusC,
		     uint32_t elemusD)
{
  int indx;
  const uint32_t vecus32x4_buf[4] = {AUS, BUS, CUS, DUS};
  uint32x4_t vecus32x4_src = vld1q_u32 (vecus32x4_buf);
  uint32x4_t vecus32x4_res = vecus32x4_src * elemusA;

  for (indx = 0; indx < 4; indx++)
    if (vecus32x4_res[indx] != expectedus4_1[indx])
      abort ();

  vecus32x4_res = vecus32x4_src * elemusB;

  for (indx = 0; indx < 4; indx++)
    if (vecus32x4_res[indx] != expectedus4_2[indx])
      abort ();

  vecus32x4_res = vecus32x4_src * elemusC;

  for (indx = 0; indx < 4; indx++)
    if (vecus32x4_res[indx] != expectedus4_3[indx])
      abort ();

  vecus32x4_res = vecus32x4_src * elemusD;

  for (indx = 0; indx < 4; indx++)
    if (vecus32x4_res[indx] != expectedus4_4[indx])
      abort ();

/* { dg-final { scan-assembler-times "\tmul\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.s\\\[0\\\]" 8 } } */
}


void
check_v4hi (int16_t elemhA, int16_t elemhB, int16_t elemhC, int16_t elemhD)
{
  int32_t indx;
  const int16_t vech16x4_buf[4] = {AH, BH, CH, DH};
  int16x4_t vech16x4_src = vld1_s16 (vech16x4_buf);
  int16x4_t vech16x4_res = vech16x4_src * elemhA;

  for (indx = 0; indx < 4; indx++)
    if (vech16x4_res[indx] != expectedh4_1[indx])
      abort ();

  vech16x4_res = vech16x4_src * elemhB;

  for (indx = 0; indx < 4; indx++)
    if (vech16x4_res[indx] != expectedh4_2[indx])
      abort ();

  vech16x4_res = vech16x4_src * elemhC;

  for (indx = 0; indx < 4; indx++)
    if (vech16x4_res[indx] != expectedh4_3[indx])
      abort ();

  vech16x4_res = vech16x4_src * elemhD;

  for (indx = 0; indx < 4; indx++)
    if (vech16x4_res[indx] != expectedh4_4[indx])
      abort ();
}

void
check_v4hi_unsigned (uint16_t elemuhA, uint16_t elemuhB, uint16_t elemuhC,
		     uint16_t elemuhD)
{
  int indx;
  const uint16_t vecuh16x4_buf[4] = {AUH, BUH, CUH, DUH};
  uint16x4_t vecuh16x4_src = vld1_u16 (vecuh16x4_buf);
  uint16x4_t vecuh16x4_res = vecuh16x4_src * elemuhA;

  for (indx = 0; indx < 4; indx++)
    if (vecuh16x4_res[indx] != expecteduh4_1[indx])
      abort ();

  vecuh16x4_res = vecuh16x4_src * elemuhB;

  for (indx = 0; indx < 4; indx++)
    if (vecuh16x4_res[indx] != expecteduh4_2[indx])
      abort ();

  vecuh16x4_res = vecuh16x4_src * elemuhC;

  for (indx = 0; indx < 4; indx++)
    if (vecuh16x4_res[indx] != expecteduh4_3[indx])
      abort ();

  vecuh16x4_res = vecuh16x4_src * elemuhD;

  for (indx = 0; indx < 4; indx++)
    if (vecuh16x4_res[indx] != expecteduh4_4[indx])
      abort ();

/* { dg-final { scan-assembler-times "mul\tv\[0-9\]+\.4h, v\[0-9\]+\.4h, v\[0-9\]+\.h\\\[0\\\]" 8 } } */
}

void
check_v8hi (int16_t elemhA, int16_t elemhB, int16_t elemhC, int16_t elemhD,
	    int16_t elemhE, int16_t elemhF, int16_t elemhG, int16_t elemhH)
{
  int32_t indx;
  const int16_t vech16x8_buf[8] = {AH, BH, CH, DH, EH, FH, GH, HH};
  int16x8_t vech16x8_src = vld1q_s16 (vech16x8_buf);
  int16x8_t vech16x8_res = vech16x8_src * elemhA;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_1[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhB;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_2[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhC;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_3[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhD;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_4[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhE;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_5[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhF;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_6[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhG;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_7[indx])
      abort ();

  vech16x8_res = vech16x8_src * elemhH;

  for (indx = 0; indx < 8; indx++)
    if (vech16x8_res[indx] != expectedh8_8[indx])
      abort ();
}

void
check_v8hi_unsigned (uint16_t elemuhA, uint16_t elemuhB, uint16_t elemuhC,
		     uint16_t elemuhD, uint16_t elemuhE, uint16_t elemuhF,
		     uint16_t elemuhG, uint16_t elemuhH)
{
  int indx;
  const uint16_t vecuh16x8_buf[8] = {AUH, BUH, CUH, DUH, EUH, FUH, GUH, HUH};
  uint16x8_t vecuh16x8_src = vld1q_u16 (vecuh16x8_buf);
  uint16x8_t vecuh16x8_res = vecuh16x8_src * elemuhA;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_1[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhB;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_2[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhC;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_3[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhD;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_4[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhE;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_5[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhF;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_6[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhG;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_7[indx])
      abort ();

  vecuh16x8_res = vecuh16x8_src * elemuhH;

  for (indx = 0; indx < 8; indx++)
    if (vecuh16x8_res[indx] != expecteduh8_8[indx])
      abort ();

/* { dg-final { scan-assembler-times "mul\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.h\\\[0\\\]" 16 } } */
}

int
main (void)
{
  check_v2sf (_elemA, _elemB);
  check_v4sf (_elemA, _elemB, _elemC, _elemD);
  check_v2df (_elemdC, _elemdD);
  check_v2si (_elemsA, _elemsB);
  check_v4si (_elemsA, _elemsB, _elemsC, _elemsD);
  check_v4hi (_elemhA, _elemhB, _elemhC, _elemhD);
  check_v8hi (_elemhA, _elemhB, _elemhC, _elemhD,
	      _elemhE, _elemhF, _elemhG, _elemhH);
  check_v2si_unsigned (_elemusA, _elemusB);
  check_v4si_unsigned (_elemusA, _elemusB, _elemusC, _elemusD);
  check_v4hi_unsigned (_elemuhA, _elemuhB, _elemuhC, _elemuhD);
  check_v8hi_unsigned (_elemuhA, _elemuhB, _elemuhC, _elemuhD,
		       _elemuhE, _elemuhF, _elemuhG, _elemuhH);

  return 0;
}

