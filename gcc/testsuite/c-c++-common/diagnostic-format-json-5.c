/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json" } */

struct s { int color; };

int test (struct s *ptr)
{
  return ptr->colour;
}

/* Use dg-regexp to consume the JSON output starting with
   the innermost values, and working outwards.
   We can't rely on any ordering of the keys.  */

/* { dg-regexp "\"kind\": \"error\"" } */
/* { dg-regexp "\"column-origin\": 1" } */
/* { dg-regexp "\"message\": \".*\"" } */

/* Verify fix-it hints.  */

/* { dg-regexp "\"string\": \"color\"" } */

/* { dg-regexp "\"start\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-5.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 15" } */
/* { dg-regexp "\"display-column\": 15" } */
/* { dg-regexp "\"byte-column\": 15" } */

/* { dg-regexp "\"next\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-5.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 21" } */
/* { dg-regexp "\"display-column\": 21" } */
/* { dg-regexp "\"byte-column\": 21" } */

/* { dg-regexp "\"fixits\": \[\[\{\}, \]*\]" } */

/* { dg-regexp "\"caret\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-5.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 15" } */
/* { dg-regexp "\"display-column\": 15" } */
/* { dg-regexp "\"byte-column\": 15" } */

/* { dg-regexp "\"finish\": \{" } */
/* { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-5.c\"" } */
/* { dg-regexp "\"line\": 8" } */
/* { dg-regexp "\"column\": 20" } */
/* { dg-regexp "\"display-column\": 20" } */
/* { dg-regexp "\"byte-column\": 20" } */

/* { dg-regexp "\"locations\": \[\[\{\}, \]*\]" } */
/* { dg-regexp "\"children\": \[\[\]\[\]\]" } */
/* { dg-regexp "\[\[\{\}, \]*\]" } */
