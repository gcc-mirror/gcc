/* PR c/20043 */
/* { dg-compile } */
/* { dg-options "-std=gnu99" } */

extern void f0 (int *);
extern void f0 (int *__restrict);

extern void f1 (int *__restrict);
extern void f1 (int *);

typedef union { int *i; long *l; } U2
  __attribute__((transparent_union));
extern void f2 (U2);
extern void f2 (int *);

typedef union { int *__restrict i; long *__restrict l; } U3
  __attribute__((transparent_union));
extern void f3 (U3);
extern void f3 (int *__restrict);

extern void f4 (U3);
extern void f4 (int *);

extern void f5 (U2);
extern void f5 (int *__restrict);

typedef union { long *l; int *i; } U6
  __attribute__((transparent_union));
extern void f6 (U6);
extern void f6 (int *);

typedef union { long *__restrict l; int *__restrict i; } U7
  __attribute__((transparent_union));
extern void f7 (U7);
extern void f7 (int *__restrict);

extern void f8 (U7);
extern void f8 (int *);

extern void f9 (U6);
extern void f9 (int *__restrict);

extern void f10 (U2);
extern void f11 (U3);
extern void f12 (U6);
extern void f13 (U7);

int i;
long l;

int
main (void)
{
  f0 (&i);
  f0 (&l);	/* { dg-warning "passing argument 1 of 'f0' from incompatible pointer type" } */
  f1 (&i);
  f1 (&l);	/* { dg-warning "passing argument 1 of 'f1' from incompatible pointer type" } */
  f2 (&i);
  f2 (&l);	/* { dg-warning "passing argument 1 of 'f2' from incompatible pointer type" } */
  f3 (&i);
  f3 (&l);	/* { dg-warning "passing argument 1 of 'f3' from incompatible pointer type" } */
  f4 (&i);
  f4 (&l);	/* { dg-warning "passing argument 1 of 'f4' from incompatible pointer type" } */
  f5 (&i);
  f5 (&l);	/* { dg-warning "passing argument 1 of 'f5' from incompatible pointer type" } */
  f6 (&i);
  f6 (&l);	/* { dg-warning "passing argument 1 of 'f6' from incompatible pointer type" } */
  f7 (&i);
  f7 (&l);	/* { dg-warning "passing argument 1 of 'f7' from incompatible pointer type" } */
  f8 (&i);
  f8 (&l);	/* { dg-warning "passing argument 1 of 'f8' from incompatible pointer type" } */
  f9 (&i);
  f9 (&l);	/* { dg-warning "passing argument 1 of 'f9' from incompatible pointer type" } */
  f10 (&i);
  f10 (&l);
  f11 (&i);
  f11 (&l);
  f12 (&i);
  f12 (&l);
  f13 (&i);
  f13 (&l);
  return 0;
}

/* { dg-message "note: expected '\[^\n'\]*' but argument is of type '\[^\n'\]*'" "note: expected" { target *-*-* } 0 } */
