/* { dg-do compile } */
/* { dg-options "" } */

/* Check compatibility of array declarations.  */

/* Incomplete decl matches.  */
extern char arr0[];
char arr0[1];

/* Two integral expressions must be the same.  Note that 0 is
   a gcc extension, but it should work like any other constant.  */
extern char arr1[1];
char arr1[1];
extern char arr2[0];
char arr2[0];
extern char arr3[0];            /* { dg-error "previous declaration" } */
char arr3[1];                   /* { dg-error "conflicting types" } */

/* Variable size matches.  */
void func(int n, int m)
{
  /* The next two are from the example in c99 6.7.5.2/9.  */
  {
    /* Invalid: not compatible because 4 != 6.  */
    int a[n][6][m];
    int (*p)[4][n+1];
    p = a;			/* { dg-error "incompatible" } */
  }
  {
    /* Compatible, but defined behavior only if n == 6 and m == n+1.  */
    int c[n][n][6][m];
    int (*r)[n][n][n+1];
    r = c;
  }
  {
    /* Compatible, but undefined behavior; (2, 2) is not a constant
       expression, and thus A is a VLA.  */
    int a[6][(2, 2)];
    int (*p)[3];
    p = a; /* { dg-bogus "incompatible" "bad vla handling" } */
  }
}
