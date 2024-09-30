/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -msoft-float -mdejagnu-cpu=power7" } */

/* PR 47151: libgcc fails to build when using --with-cpu=power7 due to a missed
   TARGET_HARD_FLOAT test.  */
unsigned long long
func (float a)
{
  const float dfa = a;
  const unsigned int hi = dfa / 0x1p32f;
  const unsigned int lo = dfa - (float) hi * 0x1p32f;
  return ((unsigned long long) hi << (4 * 8)) | lo;
}
