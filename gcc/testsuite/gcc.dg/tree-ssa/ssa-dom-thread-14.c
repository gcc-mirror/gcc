/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2-details -w --param logical-op-non-short-circuit=1" } */
/* { dg-additional-options "-fdisable-tree-thread1 -fdisable-tree-ethread -fdisable-tree-thread2" } */

enum optab_methods
{
  OPTAB_DIRECT,
  OPTAB_LIB,
  OPTAB_WIDEN,
  OPTAB_LIB_WIDEN,
  OPTAB_MUST_WIDEN
};
struct optab_d { };
typedef struct optab_d *optab;
void
expand_shift_1 (int code, int unsignedp, int rotate,
		optab lshift_optab, optab rshift_arith_optab)
{
  int left = (code == 42 || code == 0xde);
  int attempt;
  enum optab_methods methods;
  if (attempt == 0)
    methods = OPTAB_DIRECT;
  else if (attempt == 1)
    methods = OPTAB_WIDEN;
  if ((!unsignedp || (!left && methods == OPTAB_WIDEN)))
    {
      enum optab_methods methods1 = methods;
      if (unsignedp)
	methods1 = OPTAB_MUST_WIDEN;
      expand_binop (left ? lshift_optab : rshift_arith_optab,
			   unsignedp, methods1);
    }
}

/* When UNSIGNEDP is true, LEFT is false and METHOD == OPTAB_WIDEN
   we will enter the TRUE arm of the conditional and we can thread
   the test to compute the first first argument of the expand_binop
   call if we look backwards through the boolean logicals.  */
/* { dg-final { scan-tree-dump-times "Threaded" 2 "dom2"} } */

