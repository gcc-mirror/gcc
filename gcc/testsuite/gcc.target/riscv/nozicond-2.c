/* { dg-do compile { target { rv64 } } } */
/* { dg-additional-options "-march=rv64gc_zicond -mabi=lp64d -mbranch-cost=4" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */


long foo1 (long c) { return c < 0 ? 1 : -1; }
long foo2 (long c) { return c >= 0 ? -1 : 1; }

/* We don't support 4->3 splitters, so this fails.  We could perhaps
   try to catch it in the expander as a special case rather than waiting
   for combine.  */
/* { dg-final { scan-assembler-times {srai\t} 2 } } */
/* { dg-final { scan-assembler-times {ori\t} 2 } } */
/* { dg-final { scan-assembler-times {not\t} 2 } } */

