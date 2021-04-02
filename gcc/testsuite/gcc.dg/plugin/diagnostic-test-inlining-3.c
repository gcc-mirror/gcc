/* { dg-do compile } */
/* { dg-options "-Wno-attributes -fdiagnostics-show-caret -O1" } */

extern void __emit_warning (const char *message);

__attribute__((always_inline))
static void foo (void)
{
  __emit_warning ("message");
}

__attribute__((always_inline))
static void bar (void)
{
  foo ();
}

int main()
{
  bar ();
  return 0;
}

/* Reproducer for PR tree-optimization/83336: when optimization is
   enabled, but debuginfo isn't, the diagnostics subsystem doesn't
   report the full inlining chain at a middle-end warning.

   This is a copy of diagnostic-test-inlining-1.c, but with -O1.

   Ideally the diagnostics subsystem would report:
     In function 'foo', inlined from 'bar' at LOC A, inlined from 'main' at LOC B:
   but with -O1 it only reports:
     In function 'foo', inlined from 'main' at LOC A:

   This test case captures this behavior.  */

/* { dg-regexp "In function 'foo'," "" } */
/* { dg-regexp "    inlined from 'bar' at .+/diagnostic-test-inlining-3.c:15:3," "" } */
/* { dg-regexp "    inlined from 'main' at .+/diagnostic-test-inlining-3.c:20:3:" "" } */
/* { dg-warning "3: message" "" { target *-*-* } 9 } */
/* { dg-begin-multiline-output "" }
   __emit_warning ("message");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
