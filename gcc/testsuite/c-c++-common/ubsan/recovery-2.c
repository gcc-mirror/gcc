/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=signed-integer-overflow -w" } */
/* { dg-shouldfail "ubsan" } */

#include "recovery-common.inc"

/* { dg-output "shift exponent 152 is too large for \[^\n\r]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 153 is too large for \[^\n\r]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 2147483647 \\+ 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*(?!.*signed integer overflow)" } */
