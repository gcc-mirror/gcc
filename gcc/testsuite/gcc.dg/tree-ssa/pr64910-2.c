/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1 -fno-ipa-icf" } */

/* We want to make sure that we reassociate in a way that has the
   constant last.  With the constant last, it's more likely to result
   in a bitfield test on targets with such capabilities.  */

extern void boo ();

int b2b_uc (unsigned char u, unsigned char w)
{
  if ((u & w) & 0x20)
    boo ();
}

int b2b_us (unsigned short u, unsigned short w)
{
  if ((u & w) & 0x20)
    boo ();
}

int b2b_ui (unsigned int u, unsigned int w)
{
  if ((u & w) & 0x20)
    boo ();
}
int b2b_ul (unsigned long u, unsigned long w)
{
  if ((u & w) & 0x20)
    boo ();
}
int b2b_ull (unsigned long long u, unsigned long long w)
{
  if ((u & w) & 0x20)
    boo ();
}

int b2b_sc (signed char u, signed char w)
{
  if ((u & w) & 0x20)
    boo ();
}

int b2b_ss (signed short u, signed short w)
{
  if ((u & w) & 0x20)
    boo ();
}

int b2b_si (signed int u, signed int w)
{
  if ((u & w) & 0x20)
    boo ();
}
int b2b_sl (signed long u, signed long w)
{
  if ((u & w) & 0x20)
    boo ();
}
int b2b_sll (signed long long u, signed long long w)
{
  if ((u & w) & 0x20)
    boo ();
}

/* The AND of U & W should go into a temporary, when is then ANDed
   with the constant.

   First verify that we have the right number of ANDs between U and W.  */
/* { dg-final { scan-tree-dump-times "\[uw\]_\[0-9\]+.D. \& \[uw\]_\[0-9\]+.D.;" 10 "reassoc1"} } */

/* Then verify that we have the right number of ANDS between a temporary
   and the constant.  */
/* { dg-final { scan-tree-dump-times "_\[0-9]+ \& 32;" 10 "reassoc1"} } */

/* Each function has one AND.  It will have either a second AND or TEST.  So
   we can count the number of AND and TEST instructions.  They must be 2X
   the number of test functions in this file.  */
/* { dg-final { scan-assembler-times "and|test" 20 { target { i?86-*-* x86_64-*-*} } } } */

/* Similarly on the m68k.  The code for the long long tests is suboptimal,
   which catch via the second pattern and xfail.  */
/* { dg-final { scan-assembler-times "and|btst" 20 { target { m68k-*-* } } } } */
/* { dg-final { scan-assembler-not "or" { target { m68k-*-* } xfail { *-*-* } } } } */

