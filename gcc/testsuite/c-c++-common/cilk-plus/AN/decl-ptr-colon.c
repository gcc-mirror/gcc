/* { dg-do compile { target c } } */
/* { dg-options "-fcilkplus" } */

int main(void)
{
  extern int func(int);
  int array3[:], x, q; /* { dg-error "array notations cannot be used in declaration" } */
  int  array3[1:2:x]; /* { dg-error "array notations cannot be used in declaration" } */
  extern char array3[1:func(x)]; /* { dg-error "array notations cannot be used in declaration" } */
  int *a, ***b;
  extern char *c;
  int array2[10];

  a[:] = 5; /* { dg-error  "start-index and length fields necessary for using array notations in pointers" } */
  c[1:2] =  3; /* This is OK.  */
  (array2)[:] = 5; /* This is OK.  */
  b[1:2][1:func(x)][:] = 3; /*  { dg-error  "start-index and length fields necessary for using array notations in pointers" }  */
}

