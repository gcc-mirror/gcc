/* Check that all more-or-less trivially fillable delayed-branch-slots
   are filled. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\tnop|addq 8,|subq 8," 2 } } */

/* The reason for the weird variant of scan-assembler-not "\tnop" is that we
   used to have an unused DWunion temp on stack in xlshrdi3 and delay-slots for
   the conditional jumps filled by the addq 8/subq setting up that, but with a
   MAX_FIXED_MODE_SIZE no longer 32, but the default 64, that stack-frame is
   eliminated, but we no longer have eligible insns to fill the delay-slots.
   Not wanting to tweak the code in the test-case, this is second best: allowing
   two nops -or- an addq 8 + subq 8 assuming code generation is otherwise
   reasonably sane.  */

void *f(void **p)
{
  /* Supposedly the memory read finds its way into the "ret"
     delay-slot. */
  return *p;
}

#if 0
/* Until the negative effects of g:897a73086b2 a.k.a. r10-6395
   a.k.a. "One more fix for PR 91333 - suboptimal register allocation
   for inline asm", which appears to have caused a "nop" (unfilled
   delay-slot) to appear for this function for CRIS-decc0rated (but not
   CRIS-cc0) and increasing one execution-path by one instruction (and
   the size of the whole function), it's left out.  It was but a mere
   attempt to expose the flaw better noticed with xlshrdi3.  It exposes
   a real issue, just less important.  FIXME: extract to separate test.  */
int g(int *x, int *y, char *v, int n)
{
  int z = *x;
  int w = *v + 31;

  /* Two branch and two return slots, all filled. */
  if (z != 23 && z != n+1)
    return *x+*y+24+w;
  return *y+24+w;
}
#endif

/* No problem with the two examples above, but with a more involved
   example, the epilogue contents matter (the condition-code register
   clobber was mistaken for a register that needed to be alive). */

struct DWstruct {int low, high;};
typedef unsigned long long DItype;
typedef unsigned int USItype;

typedef union
{
  struct DWstruct s;
  DItype ll;
} DWunion;

unsigned long long
xlshrdi3 (DItype u, unsigned int b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const int bm = (4 * 8) - b;
  DWunion w;

  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (USItype) uu.s.high >> -bm;
    }
  else
    {
      const USItype carries = (USItype) uu.s.high << bm;
      w.s.high = (USItype) uu.s.high >> b;
      w.s.low = ((USItype) uu.s.low >> b) | carries;
    }

  return w.ll;
}
