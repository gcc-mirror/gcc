/* { dg-do compile } */
/* { dg-options "-Wno-attributes -fdiagnostics-show-caret" } */

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

/* Verify that the diagnostic subsytem describes the chain of inlining
   when reporting the warning.  */

/* { dg-regexp "In function 'foo'," "" } */
/* { dg-regexp "    inlined from 'bar' at .+/diagnostic-test-inlining-1.c:15:3," "" } */
/* { dg-regexp "    inlined from 'main' at .+/diagnostic-test-inlining-1.c:20:3:" "" } */
/* { dg-warning "3: message" "" { target *-*-* } 9 } */
/* { dg-begin-multiline-output "" }
   __emit_warning ("message");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
