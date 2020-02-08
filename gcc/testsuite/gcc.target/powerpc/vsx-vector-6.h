/* This test code is included into vsx-vector-6.p7.c, vsx-vector-6.p8.c
   and vsx-vector-6.p9.c.  The .c files have the tests for the number
   of instructions generated for each cpu type.  */

#include <altivec.h>

typedef struct {
  vector double d;
  vector float f;
  vector long sl;
  vector int si;
  vector short ss;
  vector char sc;
  vector unsigned int ui;
  vector unsigned short int us;
  vector unsigned char uc;
  vector bool long long bll;
  vector bool long bl;
  vector bool int bi;
  vector bool short bs;
  vector bool char bc;
} opnd_t;

void
func_1op (opnd_t *dst, opnd_t *src)
{
  dst[0].d = vec_abs (src[0].d);
  dst[1].d = vec_ceil (src[1].d);
  dst[2].d = vec_floor (src[2].d);
  dst[3].d = vec_nearbyint (src[3].d);
  dst[4].d = vec_rint (src[4].d);
  dst[5].d = vec_sqrt (src[5].d);
  dst[6].d = vec_trunc (src[6].d);
  dst[7].f = vec_trunc (src[7].f);
}

void
func_2op (opnd_t *dst, opnd_t *src0, opnd_t *src1)
{
  dst[0].d = vec_add (src0[0].d, src1[0].d);
  dst[1].d = vec_div (src0[1].d, src1[1].d);
  dst[2].d = vec_max (src0[2].d, src1[2].d);
  dst[3].uc = vec_max (src0[3].uc, src1[3].uc);
  dst[4].d = vec_min (src0[4].d, src1[4].d);
  dst[5].d = vec_mul (src0[5].d, src1[5].d);
  dst[6].d = vec_sub (src0[6].d, src1[6].d);
}

void
func_2lop (opnd_t *dst, opnd_t *src0, opnd_t *src1)
{
  dst[0].d = vec_and (src0[0].d, src1[0].d);
  dst[1].d = vec_and (src0[1].d, src1[1].bl);
  dst[2].d = vec_and (src0[2].bl, src1[2].d);

  dst[3].d = vec_andc (src0[3].d, src1[3].d);
  dst[4].d = vec_andc (src0[4].d, src1[4].bl);
  dst[5].d = vec_andc (src0[5].bl, src1[5].d);
  dst[6].d = vec_andc (src0[6].bll, src1[6].d);
  dst[7].d = vec_andc (src0[7].d, src1[7].bll);
  dst[8].bi = vec_andc (src0[8].bi, src1[8].bi);
  dst[9].bs = vec_andc (src0[9].bs, src1[9].bs);
  dst[10].bc = vec_andc (src0[10].bc, src1[10].bc);
  dst[11].f = vec_andc (src0[11].f, src1[11].f);
  dst[12].f = vec_andc (src0[12].bi, src1[12].f);
  dst[13].f = vec_andc (src0[13].f, src1[13].bi);
  dst[14].d = vec_andc (src0[14].bll, src1[14].d);
  dst[15].d = vec_andc (src0[15].d, src1[15].bll);

  dst[16].d = vec_nor (src0[16].d, src1[16].d);
  dst[17].f = vec_nor (src0[17].f, src1[17].f);
  dst[18].bi = vec_nor (src0[18].bi, src1[18].bi);
  dst[19].bs = vec_nor (src0[19].bs, src1[19].bs);
  dst[20].bc = vec_nor (src0[20].bc, src1[20].bc);

  dst[21].d = vec_or (src0[21].d, src1[21].d);
  dst[22].d = vec_or (src0[22].d, src1[22].bl);
  dst[23].d = vec_or (src0[23].bl, src1[23].d);
  dst[24].d = vec_or (src0[24].bll, src1[24].d);
  dst[25].d = vec_or (src0[25].d, src1[25].bll);
  dst[26].f = vec_or (src0[26].f, src1[26].f);
  dst[27].bi = vec_or (src0[27].bi, src1[27].bi);
  dst[28].bs = vec_or (src0[28].bs, src1[28].bs);
  dst[29].bc = vec_or (src0[29].bc, src1[29].bc);

  dst[30].d = vec_xor (src0[30].d, src1[30].d);
  dst[31].d = vec_xor (src0[31].d, src1[31].bl);
  dst[32].d = vec_xor (src0[32].bl, src1[32].d);
}

void
func_cmp (opnd_t *dst, opnd_t *src0, opnd_t *src1)
{
  dst[0].bl = vec_cmpeq (src0[0].d, src1[0].d);
  dst[1].bl = vec_cmpgt (src0[1].d, src1[1].d);
  dst[2].bl = vec_cmpge (src0[2].d, src1[2].d);
  dst[3].bl = vec_cmplt (src0[3].d, src1[3].d);
  dst[4].bl = vec_cmple (src0[4].d, src1[4].d);
}

void
func_all_cmp (int *dst, opnd_t *src0, opnd_t *src1)
{
  dst[0] = vec_all_eq (src0[0].d, src1[0].d);
  dst[1] = vec_all_ge (src0[1].d, src1[1].d);
  dst[2] = vec_all_gt (src0[2].d, src1[2].d);
  dst[3] = vec_all_le (src0[3].d, src1[3].d);
  dst[4] = vec_all_lt (src0[4].d, src1[4].d);
  dst[5] = vec_all_nan (src0[5].d);
  dst[6] = vec_all_ne (src0[6].d, src1[6].d);
  dst[7] = vec_all_nge (src0[7].d, src1[7].d);
  dst[8] = vec_all_ngt (src0[8].d, src1[8].d);
  dst[9] = vec_all_nle (src0[9].d, src1[9].d);
  dst[10] = vec_all_nlt (src0[10].d, src1[10].d);
  dst[11] = vec_all_numeric (src0[11].d);
  dst[12] = vec_any_eq (src0[12].d, src1[12].d);
  dst[13] = vec_any_ge (src0[13].d, src1[13].d);
  dst[14] = vec_any_gt (src0[14].d, src1[14].d);
  dst[15] = vec_any_le (src0[15].d, src1[15].d);
  dst[16] = vec_any_lt (src0[16].d, src1[16].d);
  dst[17] = vec_any_nan (src0[17].d);
  dst[18] = vec_any_ne (src0[18].d, src1[18].d);
  dst[19] = vec_any_nge (src0[19].d, src1[19].d);
  dst[20] = vec_any_ngt (src0[20].d, src1[20].d);
  dst[21] = vec_any_nle (src0[21].d, src1[21].d);
  dst[22] = vec_any_nlt (src0[22].d, src1[22].d);
  dst[23] = vec_any_numeric (src0[23].d);
}

void
func_3op (opnd_t *dst, opnd_t *src0, opnd_t *src1, opnd_t *src2)
{
  dst[0].d = vec_madd (src0[0].d, src1[0].d, src2[0].d);
  dst[1].d = vec_msub (src0[1].d, src1[1].d, src2[1].d);
  dst[2].d = vec_nmadd (src0[2].d, src1[2].d, src2[2].d);
  dst[3].d = vec_nmsub (src0[3].d, src1[3].d, src2[3].d);

  dst[4].f = vec_madd (src0[4].f, src1[4].f, src2[4].f);
  dst[5].f = vec_msub (src0[5].f, src1[5].f, src2[5].f);
  dst[6].f = vec_nmsub (src0[6].f, src1[6].f, src2[6].f);
  dst[7].f = vec_nmadd (src0[7].f, src1[7].f, src2[7].f);

#if defined (__BIG_ENDIAN__) || defined (_ARCH_PWR9)
  dst[8].d = vec_perm (src0[8].d, src1[8].d, src2[8].uc);
#else
  dst[8].d = vec_perm (src0[8].d, src1[8].d, ~src2[8].uc);
#endif

  dst[9].d = vec_sel (src0[9].d, src1[9].d, src2[9].d);
  dst[10].d = vec_sel (src0[10].d, src1[10].d, src2[10].bl);

  dst[11].si = vec_msums(src0[11].ss, src1[11].ss, src2[11].si);
  dst[12].ui = vec_msums(src0[12].us, src1[12].us, src2[12].ui);
}
