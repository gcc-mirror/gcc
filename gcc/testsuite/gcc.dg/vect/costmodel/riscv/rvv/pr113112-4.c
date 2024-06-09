/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -Ofast -ftree-vectorize -mrvv-max-lmul=dynamic -mrvv-vector-bits=zvl -fno-schedule-insns -fno-schedule-insns2" } */

typedef struct rtx_def *rtx;
struct replacement {
    rtx *where;
    rtx *subreg_loc;
    int mode;
};
static struct replacement replacements[150];
void move_replacements (rtx *x, rtx *y, int n_replacements)
{
  int i;
  for (i = 0; i < n_replacements; i++)
    if (replacements[i].subreg_loc == x)
      replacements[i].subreg_loc = y;
    else if (replacements[i].where == x) 
      {
	replacements[i].where = y;
	replacements[i].subreg_loc = 0;
      }
}

/* { dg-final { scan-assembler-not {e64,m2} } } */
/* { dg-final { scan-assembler {e64,m4} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler {ret} } } */
/* { dg-final { scan-assembler-not {sp} } } */
