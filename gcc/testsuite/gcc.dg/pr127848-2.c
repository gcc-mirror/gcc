/* PR middle-end/127848 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* For bitint_ext_undef targets, narrowing or same precision _BitInt conversion within the same mode and size is ok.  */
/* { dg-final { scan-tree-dump-times "= foo \\\(\\\); \\\[tail call\\\]" 3 "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* } } } */
/* { dg-final { scan-tree-dump-times "= qux \\\(\\\); \\\[tail call\\\]" 3 "optimized" { target i?86-*-* x86_64*-*-* aarch64*-*-* powerpc*-*-* } } } */
/* For bitint_ext_full targets, such conversions are definitely not ok.  In these cases even on bitint_ext_partial.  */
/* { dg-final { scan-tree-dump-not "= foo \\\(\\\); \\\[tail call\\\]" "optimized" { target arm*-*-* riscv*-*-* s390*-*-* loongarch*-*-* } } } */
/* { dg-final { scan-tree-dump-not "= qux \\\(\\\); \\\[tail call\\\]" "optimized" { target arm*-*-* riscv*-*-* s390*-*-* loongarch*-*-* } } } */

unsigned _BitInt(15) foo ();
[[gnu::noipa]] unsigned _BitInt(13) bar () { return foo (); }
[[gnu::noipa]] signed _BitInt(13) baz () { return foo (); }
[[gnu::noipa]] signed _BitInt(15) corge () { return foo (); }
signed _BitInt(15) qux ();
[[gnu::noipa]] signed _BitInt(13) fred () { return qux (); }
[[gnu::noipa]] unsigned _BitInt(13) waldo () { return qux (); }
[[gnu::noipa]] unsigned _BitInt(13) xyzzy () { return qux (); }
