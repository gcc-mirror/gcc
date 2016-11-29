/* PR c/65467 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c11" } */

void
f1 (void)
{
  struct S { int a; int b[2]; _Atomic int *c; };
  _Atomic int a = 0, b[2];
  _Atomic int d[3];
  _Atomic struct S c = (struct S) { 3, { 4, 5 }, d };
  int *_Atomic p;
  _Atomic int *q;
  int e[3] = { 1, 2, 3 };
  b[0] = 1;
  b[1] = 2;
  d[0] = 6;
  d[1] = 7;
  d[2] = 8;
  p = e;
  #pragma omp target map(tofrom: a)		/* { dg-error "'_Atomic' 'a' in 'map' clause" } */
  ;
  #pragma omp target map(to: b)			/* { dg-error "'_Atomic' 'b' in 'map' clause" } */
  ;
  #pragma omp target map(from: b[1:1])		/* { dg-error "'_Atomic' 'b' in 'map' clause" } */
  ;
  #pragma omp target map(to: c.a)		/* { dg-error "'_Atomic' 'c.a' in 'map' clause" } */
  /* { dg-warning "accessing a member 'a' of an atomic structure 'c'" "" { target *-*-* } 27 } */
  ;
  #pragma omp target map(to: c.b[1])		/* { dg-error "'_Atomic' 'c.b' in 'map' clause" } */
  /* { dg-warning "accessing a member 'b' of an atomic structure 'c'" "" { target *-*-* } 30 } */
  ;
  #pragma omp target data map(c)		/* { dg-error "'_Atomic' 'c' in 'map' clause" } */
  /* { dg-error "must contain at least one" "" { target *-*-* } 33 } */
  {
    #pragma omp target update to (c.a)		/* { dg-error "'_Atomic' 'c.a' in 'to' clause" } */
    /* { dg-error "must contain at least one" "" { target *-*-* } 36 } */
    /* { dg-warning "accessing a member 'a' of an atomic structure 'c'" "" { target *-*-* } 36 } */
    #pragma omp target update from (c.b[1])	/* { dg-error "'_Atomic' 'c.b' in 'from' clause" } */
    /* { dg-error "must contain at least one" "" { target *-*-* } 39 } */
    /* { dg-warning "accessing a member 'b' of an atomic structure 'c'" "" { target *-*-* } 39 } */
    #pragma omp target update to (c)		/* { dg-error "'_Atomic' 'c' in 'to' clause" } */
    /* { dg-error "must contain at least one" "" { target *-*-* } 42 } */
  }
  #pragma omp target map(to: c.c[0:])		/* { dg-error "'_Atomic' 'c.c' in 'map' clause" } */
  /* { dg-warning "accessing a member 'c' of an atomic structure 'c'" "" { target *-*-* } 45 } */
  ;
  #pragma omp target map(to: p[1:2])		/* { dg-error "'_Atomic' 'p' in 'map' clause" } */
  ;
  #pragma omp target map(to: q[1:2])		/* { dg-error "'_Atomic' '\[^\n\r]*' in 'map' clause" } */
  ;
}

void
f2 (void)
{
  _Atomic int a = 0, b[2] = { 1, 2 };
  #pragma omp target defaultmap(tofrom:scalar)	/* { dg-error "'_Atomic' 'a' in implicit 'map' clause" } */
  a++;
  #pragma omp target				/* { dg-error "'_Atomic' 'b' in implicit 'map' clause" } */
  b[0]++;
}

void
f3 (void)
{
  _Atomic int a = 0, b[2] = { 1, 2 };
  #pragma omp target				/* { dg-error "'_Atomic' 'a' in implicit 'firstprivate' clause on 'target' construct" } */
  a++;
  #pragma omp target firstprivate (a)		/* { dg-error "'_Atomic' 'a' in 'firstprivate' clause on 'target' construct" } */
  a++;
  #pragma omp target firstprivate (b)		/* { dg-error "'_Atomic' 'b' in 'firstprivate' clause on 'target' construct" } */
  b[0]++;
}
