/* Missing <stddef.h>.  */

void *ptr = NULL; /* { dg-error "'NULL' undeclared here" } */
/* { dg-message "'NULL' is defined in header '<stddef.h>'; this is probably fixable by adding '#include <stddef.h>'" "" { target *-*-* } .-1 } */

ptrdiff_t pd; /* { dg-error "unknown type name 'ptrdiff_t'" } */
/* { dg-message "'ptrdiff_t' is defined in header '<stddef.h>'; this is probably fixable by adding '#include <stddef.h>'" "" { target *-*-* } .-1 } */

wchar_t wc; /* { dg-error "unknown type name 'wchar_t'" } */
/* { dg-message "'wchar_t' is defined in header '<stddef.h>'; this is probably fixable by adding '#include <stddef.h>'" "" { target *-*-* } .-1 } */

size_t sz; /* { dg-error "unknown type name 'size_t'" } */
/* { dg-message "'size_t' is defined in header '<stddef.h>'; this is probably fixable by adding '#include <stddef.h>'" "" { target *-*-* } .-1 } */

/* Missing <stdio.h>.  */

void test_stdio_h (void)
{
  FILE *f; /* { dg-error "unknown type name 'FILE'" } */
  /* { dg-message "'FILE' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */

  char buf[BUFSIZ]; /* { dg-error "'BUFSIZ' undeclared" } */
  /* { dg-message "'BUFSIZ' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */

  char buf2[FILENAME_MAX]; /* { dg-error "'FILENAME_MAX' undeclared" } */
  /* { dg-message "'FILENAME_MAX' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */

  stderr; /* { dg-error "'stderr' undeclared" } */
  /* { dg-message "'stderr' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */

  stdin; /* { dg-error "'stdin' undeclared" } */
  /* { dg-message "'stdin' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */

  stdout; /* { dg-error "'stdout' undeclared" } */
  /* { dg-message "'stdout' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */

  EOF; /* { dg-error "'EOF' undeclared" } */
  /* { dg-message "'EOF' is defined in header '<stdio.h>'; this is probably fixable by adding '#include <stdio.h>'" "" { target *-*-* } .-1 } */
}

/* Missing <stdlib.h>.  */

void test_stdlib (int i)
{
  i = EXIT_SUCCESS; /* { dg-error "'EXIT_SUCCESS' undeclared" } */
  /* { dg-message "'EXIT_SUCCESS' is defined in header '<stdlib.h>'; this is probably fixable by adding '#include <stdlib.h>'" "" { target *-*-* } .-1 } */
  i = EXIT_FAILURE; /* { dg-error "'EXIT_FAILURE' undeclared" } */
  /* { dg-message "'EXIT_FAILURE' is defined in header '<stdlib.h>'; this is probably fixable by adding '#include <stdlib.h>'" "" { target *-*-* } .-1 } */
}

/* Missing <errno.h>.  */

int test_errno_h (void)
{
  return errno; /* { dg-error "'errno' undeclared" } */
  /* { dg-message "'errno' is defined in header '<errno.h>'; this is probably fixable by adding '#include <errno.h>'" "" { target *-*-* } .-1 } */
}

/* Missing <stdarg.h>.  */

void test_stdarg_h (void)
{
  va_list ap; /* { dg-error "unknown type name 'va_list'" } */
  /* { dg-message "'va_list' is defined in header '<stdarg.h>'; this is probably fixable by adding '#include <stdarg.h>'" "" { target *-*-* } .-1 } */
}

/* Missing <limits.h>.  */
int test_INT_MAX (void)
{
  return INT_MAX; /* { dg-line INT_MAX_line } */
  /* { dg-error "'INT_MAX' undeclared" "" { target *-*-* } INT_MAX_line } */
  /* { dg-bogus "__INT_MAX__" "" { target *-*-* } INT_MAX_line } */
  /* { dg-message "'INT_MAX' is defined in header '<limits.h>'; this is probably fixable by adding '#include <limits.h>'" "" { target *-*-* } INT_MAX_line } */
}

/* Missing <float.h>.  */
float test_FLT_MAX = FLT_MAX; /* { dg-line FLT_MAX_line } */
/* { dg-error "'FLT_MAX' undeclared" "" { target *-*-* } FLT_MAX_line } */
/* { dg-message "'FLT_MAX' is defined in header '<float.h>'; this is probably fixable by adding '#include <float.h>'" "" { target *-*-* } FLT_MAX_line } */
