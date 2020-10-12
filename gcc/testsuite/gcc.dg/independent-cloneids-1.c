/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-rtl-final"  } */
/* { dg-skip-if "Odd label definition syntax" { mmix-*-* } } */

extern int printf (const char *, ...);

static int __attribute__ ((noinline))
foo (int arg)
{
  return 7 * arg;
}

static int __attribute__ ((noinline))
bar (int arg)
{
  return arg * arg;
}

int
baz (int arg)
{
  printf("%d\n", bar (3));
  printf("%d\n", bar (4));
  printf("%d\n", foo (5));
  printf("%d\n", foo (6));
  /* adding or removing the following call should not affect foo
     function's clone numbering */
  printf("%d\n", bar (7));
  return foo (8);
}

/* { dg-final { scan-rtl-dump-times {(?n)^;; Function bar.constprop \(bar[.$_]constprop[.$_]0,} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function bar.constprop \(bar[.$_]constprop[.$_]1,} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function bar.constprop \(bar[.$_]constprop[.$_]2,} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function foo.constprop \(foo[.$_]constprop[.$_]0,} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function foo.constprop \(foo[.$_]constprop[.$_]1,} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function foo.constprop \(foo[.$_]constprop[.$_]2,} 1 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function foo.constprop \(foo[.$_]constprop[.$_]3,} 0 "final" } } */
/* { dg-final { scan-rtl-dump-times {(?n)^;; Function foo.constprop \(foo[.$_]constprop[.$_]4,} 0 "final" } } */
