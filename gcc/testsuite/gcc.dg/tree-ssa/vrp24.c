/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-forwprop -fdump-tree-evrp-details -fdump-tree-optimized -fno-tree-ccp" } */


struct rtx_def;
typedef struct rtx_def *rtx;
union rtunion_def
{
  rtx rt_rtx;
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  int code;
  union u
  {
    rtunion fld[1];
  } u;
};

void arf (void);
void nit (void);
void frob (void);

void
sss (rtx insn, int code1, int code2, int code3)
{
  _Bool D1562;
  struct rtx_def * body;
  int i;
  int n_sets;
  int D1544;

  body = insn->u.fld[5].rt_rtx;
  D1544 = body->code;
  n_sets = 1;
  if (D1544 == 55) goto L7; else goto L1;

L1:
  n_sets = 0;
  if (code3 == 99) goto L2; else goto L11;

L2:
  D1562 = code1 == 10;
  n_sets = (int) D1562;
  if (n_sets > 0) goto L7; else goto L11;

L37:
  if (code2 == 42) goto L8; else goto L9;

L8:
  arf ();

L9:
  i = i + 1;
  if (i < n_sets) goto L37; else goto L32;

L32:

L11:
  if (n_sets > 1) goto L12; else goto L15;

L12:
  nit ();

L14:
  i = 0;
  goto L38;

L15:
  if (n_sets > 0) goto L14; else goto L16;

L38:
  frob ();
  i = i + 1;
  if (n_sets > i) goto L38; else goto L16;

L16:
  return;

L7:
  i = 0;
  goto L37;

}

/* The first n_sets > 0 test can be simplfiied into n_sets == 1 since
   n_sets can only have the values [0, 1] as it's the result of a
   boolean operation.  */

/* { dg-final { scan-tree-dump-times "Simplified relational" 2 "evrp" } } */
/* { dg-final { scan-tree-dump-times "if " 4 "optimized" } } */

