/* Test for warnings for missing format attributes.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-Wmissing-format-attribute" } */
/* { dg-options "-Wmissing-format-attribute -Wno-abi" { target arm_eabi } } */
/* VxWorks does not provide vscanf, either in kernel or RTP mode.  */
/* { dg-error "not declared" "" { target *-*-vxworks* } 26 } */

#include <stdio.h>
#include <stdarg.h>

void
foo (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vprintf (fmt, ap); /* { dg-warning "candidate" "printf attribute warning" } */
  va_end (ap);
}

void
bar (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vscanf (fmt, ap); /* { dg-warning "candidate" "scanf attribute warning" { xfail *-*-vxworks* } } */
  va_end (ap);
}

__attribute__((__format__(__printf__, 1, 2))) void
foo2 (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
}

void
vfoo (const char *fmt, va_list arg)
{
  vprintf (fmt, arg); /* { dg-warning "candidate" "printf attribute warning 2" } */
}
