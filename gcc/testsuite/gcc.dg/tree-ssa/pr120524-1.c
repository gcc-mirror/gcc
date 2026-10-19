/* PR tree-optimization/120524 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Verify (a >> 127) & 1 is optimized to (unsigned __int128)a >> 127.
   The mask 1 covers all sign-extended bits, so the arithmetic right shift
   can be converted to a logical right shift.  */
__int128 f1(__int128 a) { return (a >> 127) & 1; }

/* Verify (a >> 120) & 0xff is similarly optimized.  */
__int128 f2(__int128 a) { return (a >> 120) & 0xff; }

/* Arithmetic right shifts should be converted to logical shifts.  All BIT_AND
   masks should be folded away.  */
/* { dg-final { scan-tree-dump "uint128_t" "optimized" } } */
/* { dg-final { scan-tree-dump-not "& 1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "& 255" "optimized" } } */
