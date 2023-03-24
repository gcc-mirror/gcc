/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json" } */

#error message

/* Use dg-regexp to consume the JSON output starting with
   the innermost values, and working outwards.  */

/* { dg-regexp "\"kind\": \"error\"" } */
/* { dg-regexp "\"column-origin\": 1" } */
/* { dg-regexp "\"escape-source\": false" } */
/* { dg-regexp "\"message\": \"#error message\"" } */

/* { dg-regexp "\"caret\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-1.c\"" } */
/* { dg-regexp "\"line\": 4" } */
/* { dg-regexp "\"column\": 2" } */
/* { dg-regexp "\"display-column\": 2" } */
/* { dg-regexp "\"byte-column\": 2" } */

/* { dg-regexp "\"finish\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-1.c\"" } */
/* { dg-regexp "\"line\": 4" } */
/* { dg-regexp "\"column\": 6" } */
/* { dg-regexp "\"display-column\": 6" } */
/* { dg-regexp "\"byte-column\": 6" } */

/* { dg-regexp "\"locations\": \[\[\{\}, \]*\]" } */
/* { dg-regexp "\"children\": \[\[\]\[\]\]" } */
/* { dg-regexp "\[\[\{\}, \]*\]" } */
