/* { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=float-cast-overflow -fsanitize-recover=float-cast-overflow -DUSE_FLOAT80 -DUSE_FLOAT128" } */
/* { dg-additional-options "-DUSE_INT128" { target int128 } } */

#include "float-cast-overflow-8.c"

/* __float80 */
/* { dg-output "value -129 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value (-129|-1) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -32769 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target int128 } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target int128 } } */
/* __float128 */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target int128 } } */
/* { dg-output "\[^\n\r]*value <unknown> is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target int128 } } */
