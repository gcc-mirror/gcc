/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce" } */
  

 

struct rtx_def;
typedef struct rtx_def *rtx;
extern const char rtx_class[];
union rtunion_def
{
  rtx rtx;
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  int code;
  rtunion fld[1];
};
static int
can_move_up (rtx insn, int n_insns)
{
  while (n_insns > 0)
    {
      insn = (((insn)->fld[1]).rtx);
      if (((rtx_class[(int) (((insn)->code))]) == 'i'))
        n_insns--;
    }
  return n_insns <= 0;
}
int
com (rtx insn, int blah)
{
  if (!can_move_up (insn, blah))
    foo ();
}

/* Cddce cannot remove possibly infinite loops and there is no way how to
   determine whether the loop in can_move_up ends.  */
/* { dg-final { scan-tree-dump "if " "cddce"} } */
