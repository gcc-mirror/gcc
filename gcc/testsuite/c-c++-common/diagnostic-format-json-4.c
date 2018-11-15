/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json -Wmisleading-indentation" } */

int test (void)
{
  if (1)
    return 3;
    return 4;
  return 5;
}

/* Use dg-regexp to consume the JSON output starting with
   the innermost values, and working outwards.
   We can't rely on any ordering of the keys.  */

/* Verify nested diagnostics.  */

/* The nested diagnostic.  */

/* { dg-regexp "\"kind\": \"note\"" } */
/* { dg-regexp "\"message\": \"...this statement, but the latter is misleadingly indented as if it were guarded by the 'if'\"" } */

/* { dg-regexp "\"caret\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 5" } */

/* { dg-regexp "\"finish\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 10" } */

/* { dg-regexp "\"locations\": \[\[\{\}, \]*\]" } */

/* The outer diagnostic.  */

/* { dg-regexp "\"kind\": \"warning\"" } */
/* { dg-regexp "\"message\": \"this 'if' clause does not guard...\"" } */
/* { dg-regexp "\"option\": \"-Wmisleading-indentation\"" } */

/* { dg-regexp "\"caret\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 6" } */
/* { dg-regexp "\"column\": 3" } */

/* { dg-regexp "\"finish\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 6" } */
/* { dg-regexp "\"column\": 4" } */

/* { dg-regexp "\"locations\": \[\[\{\}, \]*\]" } */

/* { dg-regexp "\"children\": \[\[\{\}, \]*\]" } */
/* { dg-regexp "\[\[\{\}, \]*\]" } */

