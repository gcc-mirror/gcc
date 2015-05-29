/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
extern void abort (void);
struct rtx_def;
typedef struct rtx_def *rtx;
enum rtx_code
{
  CALL_INSN,
  EXPR_LIST,
  NOTE,
  LAST = 256
};

struct rtx_def
{

  enum rtx_code code:16;
};

extern void blah (rtx *);

int
nonlocal_mentioned_p (x)
     rtx x;
{
  if (x->code == CALL_INSN)
    {
      rtx const _rtx = ((x));
      if (_rtx->code != CALL_INSN
	  && _rtx->code != NOTE
	  && _rtx->code != EXPR_LIST)
	abort ();
    }

  blah (&x);
}

/* There should be no casts to a short unsigned int since the entire
   set of conditionals should optimize away.  */
/* { dg-final { scan-tree-dump-times "\\(short unsigned int\\)" 0 "dom2"} } */
                                                                                
/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dom2"} } */
                                                                                
