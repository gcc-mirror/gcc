/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 " } */

/* Previous versions of this test required that the assembler does not
   contain xxpermdi or xxswapd.  However, with the more sophisticated
   code generation used today, it is now possible that xxpermdi (aka
   xxswapd) show up without being part of a lxvd2x or stxvd2x
   sequence.  */

#include <altivec.h>

extern void abort (void);

vector int x;
const vector int y = { 0, 1, 2, 3 };
vector int z;

vector int
foo (void)
{
  return y;			/* Remove 1 swap and use lvx.  */
}

vector int
foo1 (void)
{
  x = y;			/* Remove 2 redundant swaps here.  */
  return x;			/* Remove 1 swap and use lvx.  */
}

void __attribute__ ((noinline))
fill_local (vector int *vp)
{
  *vp = x;			/* Remove 2 redundant swaps here.  */
}

/* Test aligned load from local.  */
vector int
foo2 (void)
{
  vector int v;

  /* Need to be clever here because v will normally reside in a
     register rather than memory.  */
  fill_local (&v);
  return v;			/* Remove 1 swap and use lvx.  */
}


/* Test aligned load from pointer.  */
vector int
foo3 (vector int *arg)
{
  return *arg;			/* Remove 1 swap and use lvx.  */
}

/* In this structure, the compiler should insert padding to assure
   that a_vector is properly aligned.  */
struct bar {
  short a_field;
  vector int a_vector;
};

vector int
foo4 (struct bar *bp)
{
  return bp->a_vector;		/* Remove 1 swap and use lvx.  */
}

/* Test aligned store to global.  */
void
baz (vector int arg)
{
  x = arg;			/* Remove 1 swap and use stvx.  */
}

void __attribute__ ((noinline))
copy_local (vector int *arg)
{
  x = *arg;			/* Remove 2 redundant swaps.  */
}


/* Test aligned store to local.  */
void
baz1 (vector int arg)
{
  vector int v;

  /* Need cleverness, because v will normally reside in a register
     rather than memory.  */
  v = arg;			/* Aligned store to local: remove 1
				   swap and use stvx.  */
  copy_local (&v);
}

/* Test aligned store to pointer.  */
void
baz2 (vector int *arg1, vector int arg2)
{
  /* Assume arg2 resides in register.  */
  *arg1 = arg2;			/* Remove 1 swap and use stvx.  */
}

void
baz3 (struct bar *bp, vector int v)
{
  /* Assume v resides in register.  */
  bp->a_vector = v;		/* Remove 1 swap and use stvx.  */
}

int
main (int argc, int *argv[])
{
  vector int fetched_value = foo ();
  if (fetched_value[0] != 0 || fetched_value[3] != 3)
    abort ();

  fetched_value = foo1 ();
  if (fetched_value[1] != 1 || fetched_value[2] != 2)
    abort ();

  fetched_value = foo2 ();
  if (fetched_value[2] != 2 || fetched_value[1] != 1)
    abort ();

  fetched_value = foo3 (&x);
  if (fetched_value[3] != 3 || fetched_value[0] != 0)
    abort ();

  struct bar a_struct;
  a_struct.a_vector = x;	/* Remove 2 redundant swaps.  */
  fetched_value = foo4 (&a_struct);
  if (fetched_value[2] != 2 || fetched_value[3] != 3)
    abort ();

  z[0] = 7;
  z[1] = 6;
  z[2] = 5;
  z[3] = 4;

  baz (z);
  if (x[0] != 7 || x[3] != 4)
    abort ();

  vector int source = { 8, 7, 6, 5 };

  baz1 (source);
  if (x[3] != 6 || x[2] != 7)
    abort ();

  vector int dest;
  baz2 (&dest, source);
  if (dest[0] != 8 || dest[1] != 7)
    abort ();

  baz3 (&a_struct, source);
  if (a_struct.a_vector[3] != 5 || a_struct.a_vector[0] != 8)
    abort ();

  return 0;
}
