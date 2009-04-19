/* restrict qualifiers on non-pointers must be diagnosed even when
   only a tag is being declared.  PR 38243.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

restrict struct s; /* { dg-error "restrict" } */
restrict union u; /* { dg-error "restrict" } */
