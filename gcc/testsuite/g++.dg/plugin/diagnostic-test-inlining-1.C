/* { dg-do compile } */
/* { dg-options "-Wno-attributes -fdiagnostics-show-caret" } */

extern void __emit_warning (const char *message);

/* Verify that the diagnostic subsytem describes the chain of inlining
   when reporting the warning.  */

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

/* { dg-regexp "In function 'void foo\\(\\)'," "" } */
/* { dg-regexp "    inlined from 'void bar\\(\\)' at .+/diagnostic-test-inlining-1.C:18:7," "" } */
/* { dg-regexp "    inlined from 'int main\\(\\)' at .+/diagnostic-test-inlining-1.C:23:7:" "" } */
/* { dg-warning "18: message" "" { target *-*-* } 12 } */
/* { dg-begin-multiline-output "" }
   __emit_warning ("message");
   ~~~~~~~~~~~~~~~^~~~~~~~~~~
   { dg-end-multiline-output "" } */
