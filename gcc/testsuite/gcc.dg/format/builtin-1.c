/* Test for format extensions.  Test that the __builtin functions get their
   default attributes even with -ffreestanding.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -ffreestanding" } */

#include "format.h"

void
foo (int i)
{
  __builtin_fprintf (stdout, "%d", i);
  __builtin_fprintf (stdout, "%ld", i); /* { dg-warning "format" "__builtin_fprintf" } */
  __builtin_printf ("%d", i);
  __builtin_printf ("%ld", i); /* { dg-warning "format" "__builtin_printf" } */

  __builtin_fprintf_unlocked (stdout, "%d", i);
  __builtin_fprintf_unlocked (stdout, "%ld", i); /* { dg-warning "format" "__builtin_fprintf_unlocked" } */
  __builtin_printf_unlocked ("%d", i);
  __builtin_printf_unlocked ("%ld", i); /* { dg-warning "format" "__builtin_printf_unlocked" } */
}
