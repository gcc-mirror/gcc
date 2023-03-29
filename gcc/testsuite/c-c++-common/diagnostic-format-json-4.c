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
   the innermost values, and working outwards.  */

/* Verify nested diagnostics.  */

/* The nested diagnostic.  */

/* { dg-regexp "\"kind\": \"note\"" } */
/* { dg-regexp "\"message\": \"...this statement, but the latter is misleadingly indented as if it were guarded by the 'if'\"" } */
/* { dg-regexp "\"escape-source\": false" } */

/* { dg-regexp "\"caret\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 5" } */
/* { dg-regexp "\"display-column\": 5" } */
/* { dg-regexp "\"byte-column\": 5" } */

/* { dg-regexp "\"finish\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 10" } */
/* { dg-regexp "\"display-column\": 10" } */
/* { dg-regexp "\"byte-column\": 10" } */

/* The outer diagnostic.  */

/* { dg-regexp "\"kind\": \"warning\"" } */
/* { dg-regexp "\"column-origin\": 1" } */
/* { dg-regexp "\"message\": \"this 'if' clause does not guard...\"" } */
/* { dg-regexp "\"escape-source\": false" } */
/* { dg-regexp "\"option\": \"-Wmisleading-indentation\"" } */
/* { dg-regexp "\"option_url\": \"https:\[^\n\r\"\]*#index-Wmisleading-indentation\"" } */

/* { dg-regexp "\"caret\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 6" } */
/* { dg-regexp "\"column\": 3" } */
/* { dg-regexp "\"display-column\": 3" } */
/* { dg-regexp "\"byte-column\": 3" } */

/* { dg-regexp "\"finish\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-4.c\"" } */
/* { dg-regexp "\"line\": 6" } */
/* { dg-regexp "\"column\": 4" } */
/* { dg-regexp "\"display-column\": 4" } */
/* { dg-regexp "\"byte-column\": 4" } */

/* More from the nested diagnostic (we can't guarantee what order the
   "file" keys are consumed).  */

/* { dg-regexp "\"locations\": \[\[\{\}, \]*\]" } */

/* More from the outer diagnostic.  */

/* { dg-regexp "\"locations\": \[\[\{\}, \]*\]" } */

/* { dg-regexp "\"children\": \[\[\{\}, \]*\]" } */
/* { dg-regexp "\[\[\{\}, \]*\]" } */

