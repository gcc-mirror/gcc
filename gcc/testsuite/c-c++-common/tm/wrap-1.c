/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-optimized" } */

void orig(void);
void xyzzy(void) __attribute__((transaction_wrap (orig)));

void foo() { __transaction_relaxed { orig (); } }

/* { dg-final { scan-tree-dump-times "xyzzy" 1 "optimized" } } */
