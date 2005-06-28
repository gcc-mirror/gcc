/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */

extern void abort (void);
struct rtx_def;
typedef struct rtx_def *rtx;


struct rtx_def
{

  int code;
  unsigned int unchanging:1;

};
static rtx current_sym_addr;

int
foo ()
{
  if (current_sym_addr->code == 42
      && (({
	       rtx _rtx = current_sym_addr;
	       if (((_rtx)->code) != 42)
	         abort ();
	       _rtx;}
	   )->unchanging))
    return 0;
}

/* There should be precisely one load of ->code.  If there is
   more than, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "->code" 1 "dom3"} } */

/* There should be two IF statements.  One for 'current_sym_addr->code == 42'.
   The other one for '(EXPR)->unchanging'.  */
/* { dg-final { scan-tree-dump-times "if " 2 "dom3"} } */

/* { dg-final { cleanup-tree-dump "dom3" } } */
