/* { dg-do compile } */

int x;

struct T {
  int arr[20];
};

struct S {
  struct T *tvec;
};

int main()
{
  struct S *s;
  /* You can't use an array section like this.  Make sure sensible errors are
     reported.  */
#pragma omp target map(s->tvec[3:5].arr[0:20])
/* { dg-error {'\(struct T \*\)&s->tvec\[3:5\]' is a pointer; did you mean to use '->'\?} "" { target *-*-* } .-1 } */
  { }
#pragma omp target map(s->tvec[5:x].arr[0:20])
/* { dg-error {'\(struct T \*\)&s->tvec\[5:x\]' is a pointer; did you mean to use '->'\?} "" { target *-*-* } .-1 } */
  { }

  return 0;
}
