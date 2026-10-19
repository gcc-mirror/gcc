/* PR tree-optimization/120524 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Left shift: (a << 120) & (0xff <<120) folds to just a << 120.  */
__int128 f1(__int128 a) { return (a << 120) & ((__int128)0xff << 120); }

/* Verify 64-bit case also works, this is folded by C frontend putting it
   here as regression guard. This should fold to (unsigned long)a >> 63.  */
__int128 f2(__int128 a) { return (a >> 63) & 1; }

/* Verify expected optimized forms: shift with no AND mask.  */
/* { dg-final { scan-tree-dump "<< 120" "optimized" } } */
/* { dg-final { scan-tree-dump ">> 63" "optimized" } } */
