/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling -fmodulo-sched" } */

enum rtx_code
{
  INSN, ADDR_VEC, ADDR_DIFF_VEC, CALL_INSN, CODE_LABEL, BARRIER, NOTE
};
typedef union rtunion_def
{
  int rtint;
  char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
}
rtunion;
typedef struct rtx_def
{
  unsigned short code;
  rtunion fld[1];
}
 *rtx;
typedef struct rtvec_def
{
  unsigned num_elem;
  rtunion elem[1];
}
 *rtvec;
extern rtx emit_barrier (void);
extern rtx emit_note (char *);

static void
copy_loop_body (rtx *map)
{
  int i;
  rtx insn, copy;
  rtx pat = copy->fld[3].rtx;

  switch (insn->code)
    {
    case INSN:
      if (insn->fld[7].rtx)
	{
	}
      else if (pat->code == ADDR_VEC || pat->code == ADDR_DIFF_VEC)
	{
	  int diff_vec_p = pat->code == ADDR_DIFF_VEC;
	  int len = pat->fld[diff_vec_p].rtvec->num_elem;
	  for (i = 0; i < len; i++)
	    pat->fld[diff_vec_p].rtvec->elem[i].rtx->fld[5].rtint++;
	}
    case CALL_INSN:
      for (i = 0; i < 64; i++)
	map[i] = 0;
    case CODE_LABEL:
    case BARRIER:
      copy = emit_barrier ();
    case NOTE:
      copy = emit_note ("x");
    }
}
void
unroll_loop (int insn_count, rtx *map)
{
  if (insn_count > 50)
    copy_loop_body (map);
}

