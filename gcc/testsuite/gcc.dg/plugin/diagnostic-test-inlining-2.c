/* { dg-do compile } */
/* { dg-options "-Wno-attributes -fdiagnostics-show-caret" } */

extern void __emit_warning (const char *message);

#define INNER_WARNING(MSG) __emit_warning (MSG)

#define OUTER_WARNING(MSG) INNER_WARNING (MSG)

__attribute__((always_inline))
static void foo (void)
{
  OUTER_WARNING ("message");
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

/* Verify that the diagnostic subsytem describes both the chains of
   inlining and of macro expansion when reporting the warning.  */

/* { dg-regexp "In function 'foo'," "" } */
/* { dg-regexp "    inlined from 'bar' at .+/diagnostic-test-inlining-2.c:19:3," "" } */
/* { dg-regexp "    inlined from 'main' at .+/diagnostic-test-inlining-2.c:24:3:" "" } */
/* { dg-warning "28: message" "" { target c } 6 } */
/* { dg-begin-multiline-output "" }
 #define INNER_WARNING(MSG) __emit_warning (MSG)
                            ^~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
/* { dg-message "28: in expansion of macro 'INNER_WARNING'" "" { target c } 8 } */
/* { dg-begin-multiline-output "" }
 #define OUTER_WARNING(MSG) INNER_WARNING (MSG)
                            ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
/* { dg-message "3: in expansion of macro 'OUTER_WARNING'" "" { target c } 13 } */
/* { dg-begin-multiline-output "" }
   OUTER_WARNING ("message");
   ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
