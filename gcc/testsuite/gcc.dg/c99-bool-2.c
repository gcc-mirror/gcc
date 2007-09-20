/* Test for _Bool bit-fields.  After TC2, the width of a _Bool
   bit-field must not extend the width (number of sign and value bits)
   of _Bool, which is implementation-defined but is 1 unless the
   implementation defines representations for values greater than 1 in
   type _Bool and allows for _Bool objects to have such types, which
   GCC does not.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct foo
{
  _Bool b : 2; /* { dg-error "width" } */
};
