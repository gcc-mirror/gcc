/* Test -Wchar-subscripts.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wchar-subscripts" } */

extern int a[];
int *p;
char c;
signed char sc;
unsigned char uc;

void
f (void)
{
  a[sc];
  a[uc];
  sc[a];
  uc[a];
  p[sc];
  p[uc];
  sc[p];
  uc[p];
  a[c]; /* { dg-warning "warning: array subscript has type 'char'" } */
  p[c]; /* { dg-warning "warning: array subscript has type 'char'" } */
  /* -Wchar-subscripts does not warn if the char is not syntactically
      the subscript.  */
  c[a];
  c[p];
}
