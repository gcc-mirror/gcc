/* PR c/64423 */
/* { dg-do compile } */
/* { dg-options "-Wchar-subscripts" } */

int a[100];

int
f (char c)
{
  return a[c]		/* { dg-warning "11:array subscript has type .char." } */
          + a[c]	/* { dg-warning "14:array subscript has type .char." } */
            + a[c];	/* { dg-warning "16:array subscript has type .char." } */
}
