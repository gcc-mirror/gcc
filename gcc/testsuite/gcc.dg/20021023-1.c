/* This testcase ICEd on x86-64 because LABEL_REF + small const was not
   considered as local_symbolic_operand.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */

typedef __builtin_va_list va_list;

static unsigned int
foo (void *a, float b, const char *c, va_list d, void *e)
{
  return -1;
}

unsigned int
bar (void *a, float b, const char *c, ...)
{
  va_list args;
  unsigned int d;

  __builtin_stdarg_start (args, c);
  d = foo (a, b, c, args, a);
  __builtin_va_end (args);
  return d;
}
