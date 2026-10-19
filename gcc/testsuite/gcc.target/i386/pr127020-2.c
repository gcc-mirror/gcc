/* PR target/127020 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=cascadelake" } */

/* The source is known to be nonnegative, so expand can use either a
   zero or a sign extension.  The zero extension only needs an xor, so
   the sign extension must cost more, or a tie makes expand choose
   cltd/cqto/sar.  */

#ifdef __x86_64__
#define HALF long long
#define DWORD __int128
#define MASK __LONG_LONG_MAX__
#else
#define HALF int
#define DWORD long long
#define MASK __INT_MAX__
#endif

DWORD g;

DWORD
foo (HALF x)
{
  return x & MASK;
}

void
bar (HALF x)
{
  g = x & MASK;
}

/* { dg-final { scan-assembler-not "cltd|cqto|sar\[lq\]" } } */
