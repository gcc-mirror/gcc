/* Test for handling of tags.  "enum foo;" is invalid after an existing
   declaration (does not redeclare the tag) as well as before: bug 107164.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic" } */

enum e1; /* { dg-warning "ISO C forbids forward references to 'enum' types" } */
enum e2 { E };
enum e2; /* { dg-warning "empty declaration of 'enum' type does not redeclare tag" } */
