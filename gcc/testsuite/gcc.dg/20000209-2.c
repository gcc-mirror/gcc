/* { dg-do compile } */
/* Distilled from glibc sources.  Tests preprocessor corner cases.  */

#define NO_PAREN(rest...) rest
#define DEFINE_CATEGORY(category, items) \
const int _nl_value_type_##category[] = { NO_PAREN items }

DEFINE_CATEGORY
(
 LC_COLLATE,
 (
   1,
   2,
   3,
  ));

DEFINE_CATEGORY(LC_CTYPE, (1, 2, 3));
