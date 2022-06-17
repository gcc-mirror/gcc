/* As per stdarg-1.c, but using <stdarg.h>, rather than hardcoded builtins.  */

#include <stdarg.h>
#include "analyzer-decls.h"

/* Unpacking a va_list.  */

static void __attribute__((noinline))
__analyzer_called_by_test_1 (int placeholder, ...)
{
  const char *s;
  int i;
  char c;
  
  va_list ap;
  va_start (ap, placeholder);

  s = va_arg (ap, char *);
  __analyzer_eval (s[0] == 'f'); /* { dg-warning "TRUE" } */

  i = va_arg (ap, int);
  __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */

  c = (char)va_arg (ap, int);
  __analyzer_eval (c == '@'); /* { dg-warning "TRUE" } */

  va_end (ap);
}

void test_1 (void)
{
  __analyzer_called_by_test_1 (42, "foo", 1066, '@');
}

/* Unpacking a va_list passed from an intermediate function.  */

static void __attribute__((noinline))
__analyzer_test_2_inner (va_list ap)
{
  const char *s;
  int i;
  char c;
  
  s = va_arg (ap, char *);
  __analyzer_eval (s[0] == 'f'); /* { dg-warning "TRUE" } */

  i = va_arg (ap, int);
  __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */

  c = (char)va_arg (ap, int);
  __analyzer_eval (c == '@'); /* { dg-warning "TRUE" } */
}

static void __attribute__((noinline))
__analyzer_test_2_middle (int placeholder, ...)
{
  va_list ap;
  va_start (ap, placeholder);
  __analyzer_test_2_inner (ap);
  va_end (ap);
}

void test_2 (void)
{
  __analyzer_test_2_middle (42, "foo", 1066, '@');
}

/* Not enough args.  */

static void __attribute__((noinline))
__analyzer_called_by_test_not_enough_args (int placeholder, ...)
{
  const char *s;
  int i;
  
  va_list ap;
  va_start (ap, placeholder);

  s = va_arg (ap, char *);
  __analyzer_eval (s[0] == 'f'); /* { dg-warning "TRUE" } */

  i = va_arg (ap, int); /* { dg-warning "'ap' has no more arguments \\(1 consumed\\)" } */

  va_end (ap);
}

void test_not_enough_args (void)
{
  __analyzer_called_by_test_not_enough_args (42, "foo");
}

/* Not enough args, with an intermediate function.  */

static void __attribute__((noinline))
__analyzer_test_not_enough_args_2_inner (va_list ap)
{
  const char *s;
  int i;
  
  s = va_arg (ap, char *);
  __analyzer_eval (s[0] == 'f'); /* { dg-warning "TRUE" } */

  i = va_arg (ap, int); /* { dg-warning "'ap' has no more arguments \\(1 consumed\\)" } */
}

static void __attribute__((noinline))
__analyzer_test_not_enough_args_2_middle (int placeholder, ...)
{
  va_list ap;
  va_start (ap, placeholder);
  __analyzer_test_not_enough_args_2_inner (ap);
  va_end (ap);
}

void test_not_enough_args_2 (void)
{
  __analyzer_test_not_enough_args_2_middle (42, "foo");
}

/* Excess args (not a problem).  */

static void __attribute__((noinline))
__analyzer_called_by_test_excess_args (int placeholder, ...)
{
  const char *s;
  
  va_list ap;
  va_start (ap, placeholder);

  s = va_arg (ap, char *);
  __analyzer_eval (s[0] == 'f'); /* { dg-warning "TRUE" } */

  va_end (ap);
}

void test_excess_args (void)
{
  __analyzer_called_by_test_excess_args (42, "foo", "bar");
}

/* Missing va_start.  */

void test_missing_va_start (int placeholder, ...)
{
  va_list ap; /* { dg-message "region created on stack here" } */
  int i = va_arg (ap, int); /* { dg-warning "use of uninitialized value 'ap'" } */
}

/* Missing va_end.  */

void test_missing_va_end (int placeholder, ...)
{
  int i;
  va_list ap;
  va_start (ap, placeholder); /* { dg-message "\\(1\\) 'va_start' called here" } */
  i = va_arg (ap, int);
} /* { dg-warning "missing call to 'va_end'" "warning" } */
/* { dg-message "\\(2\\) missing call to 'va_end' to match 'va_start' at \\(1\\)" "final event" { target *-*-* } .-1 } */

/* Missing va_end due to error-handling.  */

int test_missing_va_end_2 (int placeholder, ...)
{
  int i, j;
  va_list ap;
  va_start (ap, placeholder); /* { dg-message "\\(1\\) 'va_start' called here" } */
  i = va_arg (ap, int);
  if (i == 42)
    {
      va_end (ap);
      return -1;
    }
  j = va_arg (ap, int);
  if (j == 1066) /* { dg-message "branch" } */
    return -1; /* { dg-message "here" } */
  va_end (ap);
  return 0;
} /* { dg-warning "missing call to 'va_end'" "warning" } */

/* va_arg after va_end.  */

void test_va_arg_after_va_end (int placeholder, ...)
{
  int i;
  va_list ap;
  va_start (ap, placeholder);
  va_end (ap); /* { dg-message "'va_end' called here" } */
  i = va_arg (ap, int); /* { dg-warning "'va_arg' after 'va_end'" } */
}

/* Type mismatch: expect int, but passed a char *.  */

static void __attribute__((noinline))
__analyzer_called_by_test_type_mismatch_1 (int placeholder, ...)
{
  int i;
  
  va_list ap;
  va_start (ap, placeholder);

  i = va_arg (ap, int); /* { dg-warning "'va_arg' expected 'int' but received '\[^\n\r\]*' for variadic argument 1 of 'ap'" } */

  va_end (ap);
}

void test_type_mismatch_1 (void)
{
  __analyzer_called_by_test_type_mismatch_1 (42, "foo");
}

/* Type mismatch: expect char *, but passed an int.  */

static void __attribute__((noinline))
__analyzer_called_by_test_type_mismatch_2 (int placeholder, ...)
{
  const char *str;
  
  va_list ap;
  va_start (ap, placeholder);

  str = va_arg (ap, const char *); /* { dg-warning "'va_arg' expected 'const char \\*' but received 'int' for variadic argument 1 of 'ap'" } */

  va_end (ap);
}

void test_type_mismatch_2 (void)
{
  __analyzer_called_by_test_type_mismatch_2 (42, 1066);
}

/* As above, but with an intermediate function.  */

static void __attribute__((noinline))
__analyzer_test_type_mismatch_3_inner (va_list ap)
{
  const char *str;
  
  str = va_arg (ap, const char *); /* { dg-warning "'va_arg' expected 'const char \\*' but received 'int' for variadic argument 1 of 'ap'" } */
}

static void __attribute__((noinline))
__analyzer_test_type_mismatch_3_middle (int placeholder, ...)
{
  va_list ap;
  va_start (ap, placeholder);

  __analyzer_test_type_mismatch_3_inner (ap);

  va_end (ap);
}

void test_type_mismatch_3 (void)
{
  __analyzer_test_type_mismatch_3_middle (42, 1066);
}

/* Multiple traversals of the args.  */

static void __attribute__((noinline))
__analyzer_called_by_test_multiple_traversals (int placeholder, ...)
{
  va_list ap;

  /* First traversal.  */
  {
    int i, j;

    va_start (ap, placeholder);

    i = va_arg (ap, int);
    __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */

    j = va_arg (ap, int);
    __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */

    va_end (ap);
  }

  /* Second traversal.  */
  {
    int i, j;

    va_start (ap, placeholder);

    i = va_arg (ap, int);
    __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */

    j = va_arg (ap, int);
    __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */

    va_end (ap);
  }
}

void test_multiple_traversals (void)
{
  __analyzer_called_by_test_multiple_traversals (0, 1066, 42);
}

/* Multiple traversals, using va_copy.  */

static void __attribute__((noinline))
__analyzer_called_by_test_multiple_traversals_2 (int placeholder, ...)
{
  int i, j;
  va_list args1;
  va_list args2;

  va_start (args1, placeholder);
  va_copy (args2, args1);

  /* First traversal.  */
  i = va_arg (args1, int);
  __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */
  j = va_arg (args1, int);
  __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */
  va_end (args1);

  /* Traversal of copy.  */
  i = va_arg (args2, int);
  __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */
  j = va_arg (args2, int);
  __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */
  va_end (args2);
}

void test_multiple_traversals_2 (void)
{
  __analyzer_called_by_test_multiple_traversals_2 (0, 1066, 42);
}

/* Multiple traversals, using va_copy after a va_arg.  */

static void __attribute__((noinline))
__analyzer_called_by_test_multiple_traversals_3 (int placeholder, ...)
{
  int i, j;
  va_list args1;
  va_list args2;

  va_start (args1, placeholder);

  /* First traversal.  */
  i = va_arg (args1, int);
  __analyzer_eval (i == 1066); /* { dg-warning "TRUE" } */

  /* va_copy after the first va_arg. */
  va_copy (args2, args1);

  j = va_arg (args1, int);
  __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */
  va_end (args1);

  /* Traversal of copy.  */
  j = va_arg (args2, int);
  __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */
  va_end (args2);
}

void test_multiple_traversals_3 (void)
{
  __analyzer_called_by_test_multiple_traversals_3 (0, 1066, 42);
}

/* va_copy after va_end.  */

void test_va_copy_after_va_end (int placeholder, ...)
{
  va_list ap1, ap2;
  va_start (ap1, placeholder);
  va_end (ap1); /* { dg-message "'va_end' called here" } */
  va_copy (ap2, ap1); /* { dg-warning "'va_copy' after 'va_end'" } */
  va_end (ap2);
}

/* leak of va_copy.  */

void test_leak_of_va_copy (int placeholder, ...)
{
  va_list ap1, ap2;
  va_start (ap1, placeholder); 
  va_copy (ap2, ap1);  /* { dg-message "'va_copy' called here" } */
  va_end (ap1);
} /* { dg-warning "missing call to 'va_end'" "warning" } */
  /* { dg-message "missing call to 'va_end' to match 'va_copy' at \\(1\\)" "final event" { target *-*-* } .-1 } */

/* double va_end.  */

void test_double_va_end (int placeholder, ...)
{
  va_list ap;
  va_start (ap, placeholder);
  va_end (ap); /* { dg-message "'va_end' called here" } */
  va_end (ap); /* { dg-warning "'va_end' after 'va_end'" } */
}

/* double va_start.  */

void test_double_va_start (int placeholder, ...)
{
  int i;
  va_list ap;
  va_start (ap, placeholder); /* { dg-message "'va_start' called here" } */
  va_start (ap, placeholder);  /* { dg-warning "missing call to 'va_end'" "warning" } */
  /* { dg-message "missing call to 'va_end' to match 'va_start' at \\(1\\)" "final event" { target *-*-* } .-1 } */
  va_end (ap);
}

/* va_copy before va_start.  */

void test_va_copy_before_va_start (int placeholder, ...)
{
  va_list ap1; /* { dg-message "region created on stack here" } */
  va_list ap2;
  va_copy (ap2, ap1); /* { dg-warning "use of uninitialized value 'ap1'" } */
  va_end (ap2);
}

/* Verify that we complain about uses of a va_list after the function 
   in which va_start was called has returned.  */

va_list global_ap;

static void __attribute__((noinline))
__analyzer_called_by_test_va_arg_after_return (int placeholder, ...)
{
  va_start (global_ap, placeholder);
  va_end (global_ap);
}

void test_va_arg_after_return (void)
{
  int i;
  __analyzer_called_by_test_va_arg_after_return (42, 1066);
  i = va_arg (global_ap, int); /* { dg-warning "dereferencing pointer 'global_ap' to within stale stack frame" } */
}
