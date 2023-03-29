/* Test for handling of tags.  "enum foo;" is invalid after an existing
   declaration (does not redeclare the tag) as well as before: bug 107164.
   Test this is not diagnosed without -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

enum e1;
enum e2 { E };
enum e2;
