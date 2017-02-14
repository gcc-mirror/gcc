/* { dg-do run } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define A -16
#define B -15
#define C -14
#define D -13
#define E -12
#define F -11
#define G -10
#define H -9

#define F16_C(a) ((__fp16) a)
#define AF F16_C (A)
#define BF F16_C (B)
#define CF F16_C (C)
#define DF F16_C (D)
#define EF F16_C (E)
#define FF F16_C (F)
#define GF F16_C (G)
#define HF F16_C (H)

#define S16_C(a) ((int16_t) a)
#define AS S16_C (A)
#define BS S16_C (B)
#define CS S16_C (C)
#define DS S16_C (D)
#define ES S16_C (E)
#define FS S16_C (F)
#define GS S16_C (G)
#define HS S16_C (H)

#define U16_C(a) ((int16_t) a)
#define AU U16_C (A)
#define BU U16_C (B)
#define CU U16_C (C)
#define DU U16_C (D)
#define EU U16_C (E)
#define FU U16_C (F)
#define GU U16_C (G)
#define HU U16_C (H)

#define P16_C(a) ((poly16_t) a)
#define AP P16_C (A)
#define BP P16_C (B)
#define CP P16_C (C)
#define DP P16_C (D)
#define EP P16_C (E)
#define FP P16_C (F)
#define GP P16_C (G)
#define HP P16_C (H)

/* Expected results for vduph_lane.  */
float16_t expected_f16 = AF;
int16_t expected_s16 = DS;
uint16_t expected_u16 = BU;
poly16_t expected_p16 = CP;

/* Expected results for vduph_laneq.  */
float16_t expected_q_f16 = EF;
int16_t expected_q_s16 = BS;
uint16_t expected_q_u16 = GU;
poly16_t expected_q_p16 = FP;

void exec_vduph_lane_f16 (void)
{
  /* vduph_lane.  */
  DECL_VARIABLE(vsrc, float, 16, 4);
  DECL_VARIABLE(vsrc, int, 16, 4);
  DECL_VARIABLE(vsrc, uint, 16, 4);
  DECL_VARIABLE(vsrc, poly, 16, 4);
  VECT_VAR_DECL (buf_src, float, 16, 4) [] = {AF, BF, CF, DF};
  VECT_VAR_DECL (buf_src, int, 16, 4) [] = {AS, BS, CS, DS};
  VECT_VAR_DECL (buf_src, uint, 16, 4) [] = {AU, BU, CU, DU};
  VECT_VAR_DECL (buf_src, poly, 16, 4) [] = {AP, BP, CP, DP};
  VLOAD (vsrc, buf_src, , int, s, 16, 4);
  VLOAD (vsrc, buf_src, , float, f, 16, 4);
  VLOAD (vsrc, buf_src, , uint, u, 16, 4);
  VLOAD (vsrc, buf_src, , poly, p, 16, 4);

  float16_t res_f = vduph_lane_f16 (VECT_VAR (vsrc, float, 16, 4), 0);
  if (* (unsigned short *) &res_f != * (unsigned short *) &expected_f16)
    abort ();

  int16_t res_s = vduph_lane_s16 (VECT_VAR (vsrc, int, 16, 4), 3);
  if (* (unsigned short *) &res_s != * (unsigned short *) &expected_s16)
    abort ();

  uint16_t res_u = vduph_lane_u16 (VECT_VAR (vsrc, uint, 16, 4), 1);
  if (* (unsigned short *) &res_u != * (unsigned short *) &expected_u16)
    abort ();

  poly16_t res_p = vduph_lane_p16 (VECT_VAR (vsrc, poly, 16, 4), 2);
  if (* (unsigned short *) &res_p != * (unsigned short *) &expected_p16)
    abort ();

  /* vduph_laneq.  */
  DECL_VARIABLE(vsrc, float, 16, 8);
  DECL_VARIABLE(vsrc, int, 16, 8);
  DECL_VARIABLE(vsrc, uint, 16, 8);
  DECL_VARIABLE(vsrc, poly, 16, 8);
  VECT_VAR_DECL (buf_src, float, 16, 8) [] = {AF, BF, CF, DF, EF, FF, GF, HF};
  VECT_VAR_DECL (buf_src, int, 16, 8) [] = {AS, BS, CS, DS, ES, FS, GS, HS};
  VECT_VAR_DECL (buf_src, uint, 16, 8) [] = {AU, BU, CU, DU, EU, FU, GU, HU};
  VECT_VAR_DECL (buf_src, poly, 16, 8) [] = {AP, BP, CP, DP, EP, FP, GP, HP};
  VLOAD (vsrc, buf_src, q, int, s, 16, 8);
  VLOAD (vsrc, buf_src, q, float, f, 16, 8);
  VLOAD (vsrc, buf_src, q, uint, u, 16, 8);
  VLOAD (vsrc, buf_src, q, poly, p, 16, 8);

  res_f = vduph_laneq_f16 (VECT_VAR (vsrc, float, 16, 8), 4);
  if (* (unsigned short *) &res_f != * (unsigned short *) &expected_q_f16)
    abort ();

  res_s = vduph_laneq_s16 (VECT_VAR (vsrc, int, 16, 8), 1);
  if (* (unsigned short *) &res_s != * (unsigned short *) &expected_q_s16)
    abort ();

  res_u = vduph_laneq_u16 (VECT_VAR (vsrc, uint, 16, 8), 6);
  if (* (unsigned short *) &res_u != * (unsigned short *) &expected_q_u16)
    abort ();

  res_p = vduph_laneq_p16 (VECT_VAR (vsrc, poly, 16, 8), 5);
  if (* (unsigned short *) &res_p != * (unsigned short *) &expected_q_p16)
    abort ();
}

int
main (void)
{
  exec_vduph_lane_f16 ();
  return 0;
}
