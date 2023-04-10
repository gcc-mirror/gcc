// PR sanitizer/108894
// { dg-do run }
// { dg-options "-fsanitize=bounds -fsanitize-recover=bounds" }
// { dg-output "index 15 out of bounds for type 'int \\\[15\\\]'\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*index 0 out of bounds for type 'int \\\[\[0-9x]*\\\]'\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*index 16 out of bounds for type 'int \\\[15\\\]'" }

#include "../../gcc.dg/ubsan/bounds-4.c"
