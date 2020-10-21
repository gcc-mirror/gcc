/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 " } */

#include <altivec.h>

extern void abort (void);

vector double x;
vector double y = { 0.1, 0.2 };
vector double z;

vector double
foo (void)
{
  return y;			/* Remove 1 swap and use lvx.  */
}

vector double
foo1 (void)
{
  x = y;			/* Remove 2 redundant swaps here.  */
  return x;			/* Remove 1 swap and use lvx.  */
}

void __attribute__ ((noinline))
fill_local (vector double *vp)
{
  *vp = x;			/* Remove 2 redundant swaps here.  */
}

/* Test aligned load from local.  */
vector double
foo2 (void)
{
  vector double v;

  /* Need to be clever here because v will normally reside in a
     register rather than memory.  */
  fill_local (&v);
  return v;			/* Remove 1 swap and use lvx.  */
}


/* Test aligned load from pointer.  */
vector double
foo3 (vector double *arg)
{
  return *arg;			/* Remove 1 swap and use lvx.  */
}

/* In this structure, the compiler should insert padding to assure
   that a_vector is properly aligned.  */
struct bar {
  short a_field;
  vector double a_vector;
};

vector double
foo4 (struct bar *bp)
{
  return bp->a_vector;		/* Remove 1 swap and use lvx.  */
}

/* Test aligned store to global.  */
void
baz (vector double arg)
{
  x = arg;			/* Remove 1 swap and use stvx.  */
}

void __attribute__ ((noinline))
copy_local (vector double *arg)
{
  x = *arg;			/* Remove 2 redundant swaps.  */
}


/* Test aligned store to local.  */
void
baz1 (vector double arg)
{
  vector double v;

  /* Need cleverness, because v will normally reside in a register
     rather than memory.  */
  v = arg;			/* Aligned store to local: remove 1
				   swap and use stvx.  */
  copy_local (&v);
}

/* Test aligned store to pointer.  */
void
baz2 (vector double *arg1, vector double arg2)
{
  /* Assume arg2 resides in register.  */
  *arg1 = arg2;			/* Remove 1 swap and use stvx.  */
}

void
baz3 (struct bar *bp, vector double v)
{
  /* Assume v resides in register.  */
  bp->a_vector = v;		/* Remove 1 swap and use stvx.  */
}

int
main (double argc, double *argv[])
{
  vector double fetched_value = foo ();
  if (fetched_value[0] != 0.1 || fetched_value[1] != 0.2)
    abort ();

  fetched_value = foo1 ();
  if (fetched_value[1] != 0.2 || fetched_value[0] != 0.1)
    abort ();

  fetched_value = foo2 ();
  if (fetched_value[0] != 0.1 || fetched_value[1] != 0.2)
    abort ();

  fetched_value = foo3 (&x);
  if (fetched_value[1] != 0.2 || fetched_value[0] != 0.1)
    abort ();

  struct bar a_struct;
  a_struct.a_vector = x;	/* Remove 2 redundant swaps.  */
  fetched_value = foo4 (&a_struct);
  if (fetched_value[1] != 0.2 || fetched_value[0] != 0.1)
    abort ();

  z[0] = 0.7;
  z[1] = 0.6;

  baz (z);
  if (x[0] != 0.7 || x[1] != 0.6)
    abort ();

  vector double source = { 0.8, 0.7 };

  baz1 (source);
  if (x[0] != 0.8 || x[1] != 0.7)
    abort ();

  vector double dest;
  baz2 (&dest, source);
  if (dest[0] != 0.8 || dest[1] != 0.7)
    abort ();

  baz3 (&a_struct, source);
  if (a_struct.a_vector[1] != 0.7 || a_struct.a_vector[0] != 0.8)
    abort ();

  return 0;
}
