/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dce7" } */
      
extern void abort (void);

struct rtx_def;
typedef struct rtx_def *rtx;
enum rtx_code
{
  UNKNOWN,
  CODE_LABEL,
  NOTE,
  LAST_AND_UNUSED_RTX_CODE = 256
};
typedef union rtunion_def rtunion;
struct rtx_def
{
  enum rtx_code code:16;
};
void
delete_dead_jumptables ()
{
  rtx insn, next;
  if (insn->code == CODE_LABEL)
    {
      rtx const _rtx = insn;
      if (_rtx->code != CODE_LABEL && _rtx->code != NOTE)
	abort ();
    }
  ;
}

/* There should be no loads of ->code.  If any exist, then we failed to
   optimize away all the IF statements and the statements feeding
   their conditions.  */
/* { dg-final { scan-tree-dump-times "->code" 0 "dce7"} } */
   
/* There should be no IF statements.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dce7"} } */

