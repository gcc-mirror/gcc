/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
  
extern void abort (void);

struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
struct rtx_def
{
  int code;
  int mode;
  unsigned int in_struct:1;
};
struct tree_common
{
  int code;
};
struct tree_decl
{
  rtx rtl;
};
union tree_node
{
  struct tree_common common;
  struct tree_decl decl;
};
rtx
store_expr (exp, target, want_value)
     tree exp;
     rtx target;
     int want_value;
{
  if (exp->common.code == 42)
    abort ();
  else if (queued_subexp_p (target))
    {
      blah (target->mode);
      if (target->code)
        abort ();
    }
  else
    {
      if (target->code && (__extension__({target;})->in_struct));
    }

  if ((target != (exp->decl.rtl
		  ? (exp->decl.rtl
		     ? exp->decl.rtl
		     : (make_decl_rtl (exp, 0), exp->decl.rtl))
		  : 0))
      && expr_size (exp))
    ;
}

/* All paths to the test "target != 0" occurring in the final IF statement
   dereference target.  Thus target can not have the value zero at that
   point and the test should have been eliminated.  */
/* ??? The dominator walker (A) doesn't merge this data at joins and
   (B) only looks at immediate dominators, and only queued_subexp_p
   immediately dominates the comparison in question.  We need something
   stronger.  */
/* { dg-final { scan-tree-dump-times "target.*!= 0" 0 "dom2" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
