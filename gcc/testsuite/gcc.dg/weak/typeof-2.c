/* Test typeof with __asm redirection. */
/* { dg-do compile } */
/* -mlongcall will cause us to place &baz3 in the CTR register.  */
/* { dg-skip-if "" { powerpc*-*-* } { "-mlongcall" } { "" } } */
/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */
/* Using -mshort-calls avoids loading the function addresses in
   registers and thus getting the counts wrong.  */
/* { dg-additional-options "-mshort-calls" { target epiphany-*-* } } */

extern int foo1 (int x) __asm ("baz1");
int bar1 (int x) { return x; }
extern __typeof (bar1) foo1 __attribute ((weak, alias ("bar1")));

extern int foo2 (int x) __attribute__ ((const));
extern __typeof (foo2) foo2 __asm ("baz2");
int bar2 (int x)
{
  return foo2 (x) + foo2 (x) + foo2 (x) + foo2 (x) + foo2 (x) + foo2 (x);
}

extern int foo3 (int x);
extern __typeof (foo3) foo3 __asm ("baz3");
int bar3 (int x)
{
  return foo3 (x) + foo3 (x) + foo3 (x) + foo3 (x) + foo3 (x) + foo3 (x);
}

// { dg-final { scan-assembler-not "foo1" } }
// { dg-final { scan-assembler "baz1" } }
// { dg-final { scan-assembler-not "foo2" } }
// { dg-final { scan-assembler "baz2" } }
// { dg-final { scan-assembler-not "baz2.*baz2.*baz2.*baz2.*baz2.*baz2" } }
// { dg-final { scan-assembler-not "foo3" } }
// SH targets put the function address into a constant pool and / or register,
// so it does not appear repeated (as much as expected) in the assembler.
// { dg-final { global target_triplet } }
// { dg-final { if [string match sh-*-* $target_triplet ] {return} } }
// { dg-final { if [string match {sh[elb1-9]*-*-*} $target_triplet ] {return} } }
// Likewise for S/390 targets
// { dg-final { if [string match s390*-*-* $target_triplet ] {return} } }
// Likewise for CRIS targets.
// { dg-final { if [string match cris-*-* $target_triplet ] {return} } }
// Likewise for m68k targets.
// { dg-final { if [string match fido-*-* $target_triplet ] {return} } }
// { dg-final { if [string match m68k-*-* $target_triplet ] {return} } }
// Likewise for moxie targets.
// { dg-final { if [string match moxie-*-* $target_triplet ] {return} } }
// Likewise for msp430 targets.
// { dg-final { if [string match msp430-*-* $target_triplet ] {return} } }
// Likewise for Visium targets.
// { dg-final { if [string match visium-*-* $target_triplet ] {return} } }
// { dg-final { scan-assembler "baz3.*baz3.*baz3.*baz3.*baz3.*baz3" } }
