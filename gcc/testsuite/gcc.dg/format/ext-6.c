/* Test for format extensions.  Test that the C99 functions get their
   default attributes in gnu89 mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wformat" } */

#include "format.h"

void
foo (int i, char *s, size_t n, int *ip, va_list v0, va_list v1, va_list v2,
     va_list v3, va_list v4, va_list v5, va_list v6, va_list v7, va_list v8,
     va_list v9, va_list v10, va_list v11, va_list v12, va_list v13)
{
  fprintf (stdout, "%d", i);
  fprintf (stdout, "%ld", i); /* { dg-warning "format" "fprintf" } */
  printf ("%d", i);
  printf ("%ld", i); /* { dg-warning "format" "printf" } */
  fprintf_unlocked (stdout, "%d", i);
  fprintf_unlocked (stdout, "%ld", i); /* { dg-warning "format" "fprintf_unlocked" } */
  printf_unlocked ("%d", i);
  printf_unlocked ("%ld", i); /* { dg-warning "format" "printf_unlocked" } */
  sprintf (s, "%d", i);
  sprintf (s, "%ld", i); /* { dg-warning "format" "sprintf" } */
  snprintf (s, n, "%d", i);
  snprintf (s, n, "%ld", i); /* { dg-warning "format" "snprintf" } */
  vfprintf (stdout, "%d", v0);
  vfprintf (stdout, "%Y", v1); /* { dg-warning "format" "vfprintf" } */
  vprintf ("%d", v2);
  vprintf ("%Y", v3); /* { dg-warning "format" "vprintf" } */
  vsprintf (s, "%d", v4);
  vsprintf (s, "%Y", v5); /* { dg-warning "format" "vsprintf" } */
  vsnprintf (s, n, "%d", v6);
  vsnprintf (s, n, "%Y", v7); /* { dg-warning "format" "vsnprintf" } */
  fscanf (stdin, "%d", ip);
  fscanf (stdin, "%ld", ip); /* { dg-warning "format" "fscanf" } */
  scanf ("%d", ip);
  scanf ("%ld", ip); /* { dg-warning "format" "scanf" } */
  sscanf (s, "%d", ip);
  sscanf (s, "%ld", ip); /* { dg-warning "format" "sscanf" } */
  vfscanf (stdin, "%d", v8);
  vfscanf (stdin, "%Y", v9); /* { dg-warning "format" "vfscanf" } */
  vscanf ("%d", v10);
  vscanf ("%Y", v11); /* { dg-warning "format" "vscanf" } */
  vsscanf (s, "%d", v12);
  vsscanf (s, "%Y", v13); /* { dg-warning "format" "vsscanf" } */
}
