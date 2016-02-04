/* PR c/68833 */
/* { dg-do compile } */
/* { dg-options "-Werror=missing-format-attribute" } */

#include <stdarg.h>

void
foo (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  __builtin_vprintf (fmt, ap); /* { dg-error "candidate" "printf attribute warning" } */
  va_end (ap);
}

/* { dg-prune-output "treated as errors" } */
