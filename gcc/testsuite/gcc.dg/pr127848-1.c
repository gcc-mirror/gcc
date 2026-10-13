/* PR middle-end/127848 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* For bitint_ext_undef targets, tail call is definitely wrong-code, bit 15 is unspecified from foo and needs to be cleared in bar.  */
/* { dg-final { scan-tree-dump-not "= foo \\\(\\\); \\\[tail call\\\]" "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* } } } */

unsigned _BitInt(15) foo ();
unsigned short bar () { return foo (); }
