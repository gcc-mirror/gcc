/* Verify that popcount(~X) is folded to PREC - popcount(X),
   eliminating the BIT_NOT_EXPR for unsigned and signed input.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* unsigned int: PREC == 32.  */
int f_uint (unsigned int x)       { return __builtin_popcount   (~x); }

/* signed int: the builtin argument conversion is a no-op conversion.  */
int f_int  (int x)                { return __builtin_popcount   (~x); }

/* unsigned long long: PREC == 64.  */
int f_ull  (unsigned long long x) { return __builtin_popcountll (~x); }

/* popcount(~x) + popcount(x) == PREC: verify the sum constant-folds.  */
int f_chain (unsigned int x) { return __builtin_popcount (~x) + __builtin_popcount (x); }

/* The BIT_NOT_EXPR must be gone; the sum in f_chain must fold to 32.  */
/* { dg-final { scan-tree-dump-not "~" "optimized" } } */
/* { dg-final { scan-tree-dump "return 32" "optimized" } } */
