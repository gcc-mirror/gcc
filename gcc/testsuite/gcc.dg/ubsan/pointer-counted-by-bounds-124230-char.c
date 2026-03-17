/* Test the attribute counted_by for pointer fields and its usage in
   bounds sanitizer.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* { dg-output "index 10 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 11 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

#define PTR_TYPE char
#include "pointer-counted-by-bounds-124230.c"
