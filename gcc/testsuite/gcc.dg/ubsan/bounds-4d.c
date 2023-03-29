/* PR sanitizer/108894 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -fsanitize-recover=bounds -fstrict-flex-arrays=3" } */
/* { dg-output "index 15 out of bounds for type 'int \\\[15\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 0 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 1 out of bounds for type 'int \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 2 out of bounds for type 'int \\\[2\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 0 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 16 out of bounds for type 'int \\\[15\\\]'" } */

#include "bounds-4.c"
