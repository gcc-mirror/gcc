/* PR c++/91741 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

typedef int T;

int
fn (int ap[])
{
  int arr[10];
  int *arr2[10];
  int *p = &arr[0];
  int r = 0;

  r += sizeof (arr) / sizeof (*arr);
  r += sizeof (arr) / sizeof (p); /* { dg-warning "expression does not compute" "" { target { ! ilp32 } } } */
  r += sizeof (arr) / sizeof p; /* { dg-warning "expression does not compute" "" { target { ! ilp32 } } } */
  r += sizeof (arr) / (sizeof p);
  r += sizeof (arr) / (sizeof (p));
  r += sizeof (arr2) / sizeof p;
  r += sizeof (arr2) / sizeof (int); /* { dg-warning "expression does not compute" "" { target { ! ilp32 } } } */
  r += sizeof (arr2) / sizeof (int *);
  r += sizeof (arr2) / sizeof (short *);
  r += sizeof (arr) / sizeof (int);
  r += sizeof (arr) / sizeof (unsigned int);
  r += sizeof (arr) / sizeof (T);
  r += sizeof (arr) / sizeof (short); /* { dg-warning "expression does not compute" } */
  r += sizeof (arr) / (sizeof (short));

  r += sizeof (ap) / sizeof (char); /* { dg-warning ".sizeof. on array function parameter" } */

  const char arr3[] = "foo";
  r += sizeof (arr3) / sizeof(char);
  r += sizeof (arr3) / sizeof(int);
  r += sizeof (arr3) / sizeof (*arr3);

  int arr4[5][5];
  r += sizeof (arr4) / sizeof (arr4[0]);
  r += sizeof (arr4) / sizeof (*arr4);
  r += sizeof (arr4) / sizeof (**arr4);
  r += sizeof (arr4) / sizeof (int *);
  r += sizeof (arr4) / sizeof (int);
  r += sizeof (arr4) / sizeof (short int);

  T arr5[10];
  r += sizeof (arr5) / sizeof (T);
  r += sizeof (arr5) / sizeof (int);
  r += sizeof (arr5) / sizeof (short); /* { dg-warning "expression does not compute" } */

  double arr6[10];
  r += sizeof (arr6) / sizeof (double);
  r += sizeof (arr6) / sizeof (float); /* { dg-warning "expression does not compute" } */
  r += sizeof (arr6) / sizeof (*arr6);

  return r;
}
