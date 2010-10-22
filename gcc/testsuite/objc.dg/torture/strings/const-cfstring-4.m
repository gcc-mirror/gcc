/* Test if constant CFStrings get placed in the correct section and that the
   layout of the object is correct for both m32 and m64.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* So far, CFString is darwin-only.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "NeXT only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-mconstant-cfstrings" } */

typedef const struct __CFString * CFStringRef;
static CFStringRef appKey = (CFStringRef) @"com.apple.soundpref";

void *foo (void)
{
  void *a = (void *)appKey;
  return a;
}

/* { dg-final { scan-assembler ".section __DATA, __cfstring" } } */
/* { dg-final { scan-assembler ".long\t___CFConstantStringClassReference\n\t.long\t1992\n\t.long\t.*\n\t.long\t19\n" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler ".quad\t___CFConstantStringClassReference\n\t.long\t1992\n\t.space 4\n\t.quad\t.*\n\t.quad\t19\n" { target { *-*-darwin* && {  lp64 } } } } } */
