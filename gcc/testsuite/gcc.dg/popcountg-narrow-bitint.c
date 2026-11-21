/* { dg-do compile { target { { aarch64*-*-* x86_64-*-* } && { bitint575 && lp64 } } } } */
/* { dg-options "-Os -fdump-tree-optimized" } */
/* { dg-additional-options "-mpopcnt" { target { x86_64-*-* } } } */

/* Narrowed to 32 bits: large _BitInt known to fit in 32 bits.  */
int f1 (unsigned _BitInt(256) x)
{
  if (x > 0xFFFFFFFFu)
    __builtin_unreachable ();
  return __builtin_popcountg (x);
}

/* Narrowed to 64 bits: large _BitInt known to fit in 64 bits.  */
int f2 (unsigned _BitInt(256) x)
{
  if (x > 0xFFFFFFFFFFFFFFFFull)
    __builtin_unreachable ();
  return __builtin_popcountg (x);
}

/* Narrowed to 32 bits: _BitInt(100) known to fit in 32 bits.  */
int f3 (unsigned _BitInt(100) x)
{
  if (x > 0xFFFFFFFFu)
    __builtin_unreachable ();
  return __builtin_popcountg (x);
}

/* All three calls take a narrowed temporary.  */
/* { dg-final { scan-tree-dump-times {\.POPCOUNT \(_[0-9]+\)} 3 "optimized" } } */

/* f1 and f3 narrow to 32 bits; f2 stays 64-bit.  */
/* { dg-final { scan-tree-dump-times {_[0-9]+ = \(unsigned int\) } 2 "optimized" } } */
