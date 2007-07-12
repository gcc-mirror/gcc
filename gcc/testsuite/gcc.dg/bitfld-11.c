/* Test for rejection of __alignof on bit-fields.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

struct { int a : 1; } x;

int r = __alignof (x.a); /* { dg-error "'__alignof' applied to a bit-field" } */
