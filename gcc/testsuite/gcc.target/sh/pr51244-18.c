/* Check that no unnecessary T bit stores are done before conditional
   branches.
   This case was extracted from the CSiBE set and contained the following
   sequence:
	cmp/hi	r1,r0
	movt	r1
	tst	r1,r1
	bt	.L12
	mov.l	@r10,r1
   In this reduced code the movt and tst insns were only present in the
   unwanted sequence.  Thus, if we see any tst or movt insns, something is
   not working as expected.  This test requires -O2 because the T bit stores
   in question will be eliminated in additional insn split passes after
   reload.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "movt|tst" } } */

typedef char Char;
typedef unsigned char Bool;
typedef unsigned char UChar;
typedef int Int32;
typedef unsigned int UInt32;
typedef short Int16;
typedef unsigned short UInt16;

static inline Bool
mainGtU (UInt32 i1, UInt32 i2, UChar* block, UInt16* quadrant, UInt32 nblock,
	 Int32* budget)
{
  Int32 k;
  UChar c1, c2;
  UInt16 s1, s2;
  k = nblock + 8;
  do
    {
      c1 = block[i1];
      c2 = block[i2];
      if (c1 != c2)
	return (c1 > c2);
      s1 = quadrant[i1];
      s2 = quadrant[i2];
      if (s1 != s2)
	return (s1 > s2);

      i1++; i2++;
      k -= 8;
   } while (k >= 0);

  return 0;
}

static inline void
mainSimpleSort (UInt32* ptr, UChar* block, UInt16* quadrant, Int32 nblock,
		Int32 lo, Int32 hi, Int32 d, Int32* budget)
{
  Int32 i, j, h, bigN, hp;
  UInt32 v;
  bigN = hi - lo + 1;
  hp = 0;
  h = 1;
  j = lo + h;
  v = ptr[j];

  while (mainGtU (ptr[j-h]+d, v+d, block, quadrant, nblock, budget))
    {
      ptr[j] = ptr[j-h];
      j = j - h;
    }
}

static inline void
mainQSort3 (UInt32* ptr, UChar* block, UInt16* quadrant, Int32 nblock,
	    Int32 loSt, Int32 hiSt, Int32 dSt, Int32* budget)
{
  Int32 unLo, unHi, ltLo, gtHi;
  Int32 sp, lo, hi, d;

  Int32 stackLo[100];
  Int32 stackHi[100];
  Int32 stackD [100];

  sp = 0;
  stackLo[sp] = loSt;
  stackHi[sp] = hiSt;
  stackD [sp] = dSt;
  lo = stackLo[sp];
  hi = stackHi[sp];
  d = stackD [sp];
  mainSimpleSort (ptr, block, quadrant, nblock, lo, hi, d, budget);
}

void
mainSort (UInt32* ptr, UChar* block, UInt16* quadrant, UInt32* ftab,
	  Int32 nblock, Int32 verb, Int32* budget)
{
  Int32 sb = 0;
  Int32 lo = ftab[sb] & (~((1 << 21)));
  Int32 hi = (ftab[sb+1] & (~((1 << 21)))) - 1;
  mainQSort3 (ptr, block, quadrant, nblock, lo, hi, 2, budget);
}
