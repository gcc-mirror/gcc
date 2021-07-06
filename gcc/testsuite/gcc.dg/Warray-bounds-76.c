/* PR tree-optimization/86650 - -Warray-bounds missing inlining context
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

static void f0 (int *p, int i)
{
  p[i] = 0;         // { dg-warning "\\\[-Warray-bounds" }
}

// Expect two instances of the text below:
// { dg-regexp "In function 'f0'," "first f0 prefix" { target *-*-* } 0 }
// { dg-regexp "In function 'f0'," "second f0 prefix" { target *-*-* } 0 }

static void f1 (int *p, int i) { f0 (p + 1, i + 1); }
static void f2 (int *p, int i) { f1 (p + 1, i + 1); }

extern int a2[2];   // { dg-note "'a2'" }

void foo (void)
{
  f1 (a2 + 1, 1);
}

// { dg-regexp " +inlined from 'foo' at \[^:\]+Warray-bounds-76.c:21:\\d+:" "inlined from foo" }

extern int a3[3];   // { dg-note "'a3'" }

void bar (void)
{
  f2 (a3 + 1, 1);
}

// { dg-regexp " +inlined from 'f1' at \[^:\]+Warray-bounds-76.c:14:\\d+," "inlined from f1" }
// { dg-regexp " +inlined from 'f2' at \[^:\]+Warray-bounds-76.c:15:\\d+," "inlined from f2" }
// { dg-regexp " +inlined from 'bar' at \[^:\]+Warray-bounds-76.c:30:\\d+:" "inlined from bar" }
