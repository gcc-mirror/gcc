/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O0 -mno-fold-gimple -dp" } */
/* { dg-prune-output "gimple folding of rs6000 builtins has been disabled." } */

#include <altivec.h>


int main ()
{
  vector long long la = {5L, 14L};
  vector long long lb = {3L, 86L};
  vector bool long long ld = {0, -1};

  vector unsigned long long ua = {5L, 14L};
  vector unsigned long long ub = {3L, 86L};
  vector bool long long ud = {0, -1};

  /*  vec_or tests generate an xxlor instruction when compiled with -O0.  The xxlor
      instructions get optimized away with higher optimization levels.
      When compiling on Linux we see the xxlor instruction used as a move in various
      places.  When compiling on AIX, a move instruction is used instead of the xxlor
      instruction.  So, these tests have been isolated into a different file to limit
      the xxlor instruction use to just the vex_or builtin tests.  */      
      
  vector long long ls = vec_or (la, lb);
  vector long long lt = vec_or (la, ld);
  vector long long lu = vec_or (ld, la);

  vector unsigned long long us = vec_or (ua, ub);
  vector unsigned long long ut = vec_or (ua, ud);
  vector unsigned long long uu = vec_or (ud, ua);

  return 0;
}

/* Expected results:
   vec_or              xxlor    */

/* { dg-final { scan-assembler-times "xxlor" 6 } } */
