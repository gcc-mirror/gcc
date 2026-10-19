/* PR middle-end/127848 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* For bitint_ext_undef targets, widening precision _BitInt conversion are not ok.  */
/* For bitint_ext_full targets, widening precisions with the same size are ok if the sign is the same.  */
/* { dg-final { scan-tree-dump-not "= foo \\\(\\\); \\\[tail call\\\]" "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* } } } */
/* { dg-final { scan-tree-dump-not "= baz \\\(\\\); \\\[tail call\\\]" "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* } } } */
/* { dg-final { scan-tree-dump-not "= fred \\\(\\\); \\\[tail call\\\]" "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* arm*-*-* riscv*-*-* s390*-*-* loongarch*-*-* } } } */
/* { dg-final { scan-tree-dump-not "= corge \\\(\\\); \\\[tail call\\\]" "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* arm*-*-* riscv*-*-* s390*-*-* loongarch*-*-* } } } */
/* { dg-final { scan-tree-dump "= foo \\\(\\\); \\\[tail call\\\]" "optimized" { target arm*-*-* riscv*-*-* s390*-*-* } } } */
/* { dg-final { scan-tree-dump "= qux \\\(\\\); \\\[tail call\\\]" "optimized" { target arm*-*-* riscv*-*-* s390*-*-* } } } */

unsigned _BitInt(25) foo ();
[[gnu::noipa]] unsigned _BitInt(31) bar () { return foo (); }
signed _BitInt(25) baz ();
[[gnu::noipa]] signed _BitInt(31) qux () { return baz (); }
unsigned _BitInt(25) fred ();
[[gnu::noipa]] signed _BitInt(31) waldo () { return fred (); }
signed _BitInt(25) corge ();
[[gnu::noipa]] unsigned _BitInt(31) xyzzy () { return corge (); }
