/* [] does not indicate a flexible array member unless it is the field
   itself being declared as an incomplete array type rather than a
   pointer or other type derived from such a type.  PR 36432.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void
f (void)
{
  int a[3];
  int (*p)[];
  struct { int (*p)[]; } s;
  p = &a;
  s.p = &a;
}
