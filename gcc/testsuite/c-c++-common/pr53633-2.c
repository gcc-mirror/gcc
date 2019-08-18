/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O2 -Wall" } */
/* Check that we do not get warnings about missing return statements
   or bogus looking noreturn functions.  */
static int __attribute__((naked))
foo (void)
{
  __asm__ ("");
}

static int __attribute__((naked,noreturn))
bar (void)
{
  __asm__ ("");
}

int foo_caller (void) { return foo (); }
int bar_caller (void) { return bar (); }
