/* { dg-do compile } */
/* { dg-options "-Wno-attributes -fdiagnostics-show-caret" } */

extern void __emit_warning (const char *message);

__attribute__((always_inline))
static void depth_0 (void)
{
  __emit_warning ("message");
}

__attribute__((always_inline))
static void depth_1 (void)
{
  depth_0 ();
}

__attribute__((always_inline))
static void depth_2 (void)
{
  depth_1 ();
}

__attribute__((always_inline))
static void depth_3 (void)
{
  depth_2 ();
}

__attribute__((always_inline))
static void depth_4 (void)
{
  depth_3 ();
}

int main()
{
  depth_4 ();
  return 0;
}

/* Verify that the diagnostic subsytem describes the chain of inlining
   when reporting the warning, for an example showing many levels of
   inlining.  */

/* { dg-regexp "In function 'depth_0'," "" } */
/* { dg-regexp "    inlined from 'depth_1' at .+/diagnostic-test-inlining-4.c:15:3," "" } */
/* { dg-regexp "    inlined from 'depth_2' at .+/diagnostic-test-inlining-4.c:21:3," "" } */
/* { dg-regexp "    inlined from 'depth_3' at .+/diagnostic-test-inlining-4.c:27:3," "" } */
/* { dg-regexp "    inlined from 'depth_4' at .+/diagnostic-test-inlining-4.c:33:3," "" } */
/* { dg-regexp "    inlined from 'main' at .+/diagnostic-test-inlining-4.c:38:3:" "" } */
/* { dg-warning "3: message" "" { target *-*-* } 9 } */
/* { dg-begin-multiline-output "" }
   __emit_warning ("message");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
