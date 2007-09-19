/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */

int thrglobalvar;
#pragma omp threadprivate (thrglobalvar)
int globalvar;
const struct S
{
  int x;
} constvar = { 8 };
struct T
{
  static T t;
  int i;
};
T T::t = { 6 };
/* const qualified type, but mutable member -> not predetermined.  */
const struct U
{
  int x;
  mutable int y;
} constmutvar = { 6, 4 };

int
foo (int x)
{
  return x;
}

int
bar (int *x)
{
  return *x;
}

int
baz (U u)
{
  return u.x;
}

int
main (void)
{
  static int thrlocvar;
#pragma omp threadprivate (thrlocvar)
  static int locvar;
  static int *p;
  int i, j, s, l;

  p = new int;
  *p = 7;
  s = 6;
  l = 0;
#pragma omp parallel for /* { dg-error "enclosing parallel" } */ \
  default (none) private (p) shared (s) 
  for (i = 0; i < 64; i++)
    {
      int k = foo (0);	/* Predetermined - private (automatic var declared */
      k++;		/* in scope of construct).  */
      thrglobalvar++;	/* Predetermined - threadprivate.  */
      thrlocvar++;	/* Predetermined - threadprivate.  */
      foo (i);		/* Predetermined - private (omp for loop variable).  */
      foo (constvar.x);	/* Predetermined - shared (const qualified type).  */
      foo (T::t.i);	/* Predetermined - shared (static data member).  */
      foo (*p);		/* *p predetermined - shared (heap allocated */
      (*p)++;		/* storage).  */
      bar (p);		/* Explicitly determined - private.  */
      foo (s);		/* Explicitly determined - shared.  */
      globalvar++;	/* { dg-error "not specified in" } */
      locvar++;		/* { dg-error "not specified in" } */
      l++;		/* { dg-error "not specified in" } */
      for (j = 0; j < 2; j++); /* { dg-error "not specified in" } */
      baz (constmutvar);/* { dg-error "not specified in" } */
    }
  return 0;
}
