/* Verify:
     * with outgoing.
     * single int register push.
     * varargs and callee-save size >= 256
     * Use 2 stack adjustments.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#define REP8(X) X,X,X,X,X,X,X,X
#define REP64(X) REP8(REP8(X))

void outgoing (__builtin_va_list, ...);

double vararg_outgoing (int x1, ...)
{
  double a1 = x1, a2 = x1 * 2, a3 = x1 * 3, a4 = x1 * 4, a5 = x1 * 5, a6 = x1 * 6;
  __builtin_va_list vl;
  __builtin_va_start (vl, x1);
  outgoing (vl, a1, a2, a3, a4, a5, a6, REP64 (1), REP8 (1));
  __builtin_va_end (vl);
  return a1 + a2 + a3 + a4 + a5 + a6;
}

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 2 } } */
