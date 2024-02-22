/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 " } */

/* Previous versions of this test required that the assembler does not
   contain xxpermdi or xxswapd.  However, with the more sophisticated
   code generation used today, it is now possible that xxpermdi (aka
   xxswapd) show up without being part of a lxvd2x or stxvd2x
   sequence.  */

#include <altivec.h>

extern void abort (void);

vector long long x;
const vector long long y = { 1024, 2048 };
vector long long z;

vector long long
foo (void)
{
  return y;			/* Remove 1 swap and use lvx.  */
}

vector long long
foo1 (void)
{
  x = y;			/* Remove 2 redundant swaps here.  */
  return x;			/* Remove 1 swap and use lvx.  */
}

void __attribute__ ((noinline))
fill_local (vector long long *vp)
{
  *vp = x;			/* Remove 2 redundant swaps here.  */
}

/* Test aligned load from local.  */
vector long long
foo2 (void)
{
  vector long long v;

  /* Need to be clever here because v will normally reside in a
     register rather than memory.  */
  fill_local (&v);
  return v;			/* Remove 1 swap and use lvx.  */
}


/* Test aligned load from pointer.  */
vector long long
foo3 (vector long long *arg)
{
  return *arg;			/* Remove 1 swap and use lvx.  */
}

/* In this structure, the compiler should insert padding to assure
   that a_vector is properly aligned.  */
struct bar {
  short a_field;
  vector long long a_vector;
};

vector long long
foo4 (struct bar *bp)
{
  return bp->a_vector;		/* Remove 1 swap and use lvx.  */
}

/* Test aligned store to global.  */
void
baz (vector long long arg)
{
  x = arg;			/* Remove 1 swap and use stvx.  */
}

void __attribute__ ((noinline))
copy_local (vector long long *arg)
{
  x = *arg;			/* Remove 2 redundant swaps.  */
}


/* Test aligned store to local.  */
void
baz1 (vector long long arg)
{
  vector long long v;

  /* Need cleverness, because v will normally reside in a register
     rather than memory.  */
  v = arg;			/* Aligned store to local: remove 1
				   swap and use stvx.  */
  copy_local (&v);
}

/* Test aligned store to pointer.  */
void
baz2 (vector long long *arg1, vector long long arg2)
{
  /* Assume arg2 resides in register.  */
  *arg1 = arg2;			/* Remove 1 swap and use stvx.  */
}

void
baz3 (struct bar *bp, vector long long v)
{
  /* Assume v resides in register.  */
  bp->a_vector = v;		/* Remove 1 swap and use stvx.  */
}

int
main (long long argc, long long *argv[])
{
  vector long long fetched_value = foo ();
  if (fetched_value[0] != 1024 || fetched_value[1] != 2048)
    abort ();

  fetched_value = foo1 ();
  if (fetched_value[1] != 2048 || fetched_value[0] != 1024)
    abort ();

  fetched_value = foo2 ();
  if (fetched_value[0] != 1024 || fetched_value[1] != 2048)
    abort ();

  fetched_value = foo3 (&x);
  if (fetched_value[1] != 2048 || fetched_value[0] != 1024)
    abort ();

  struct bar a_struct;
  a_struct.a_vector = x;	/* Remove 2 redundant swaps.  */
  fetched_value = foo4 (&a_struct);
  if (fetched_value[1] != 2048 || fetched_value[0] != 1024)
    abort ();

  z[0] = 7096;
  z[1] = 6048;

  baz (z);
  if (x[0] != 7096 || x[1] != 6048)
    abort ();

  vector long long source = { 8192, 7096};

  baz1 (source);
  if (x[0] != 8192 || x[1] != 7096)
    abort ();

  vector long long dest;
  baz2 (&dest, source);
  if (dest[0] != 8192 || dest[1] != 7096)
    abort ();

  baz3 (&a_struct, source);
  if (a_struct.a_vector[1] != 7096 || a_struct.a_vector[0] != 8192)
    abort ();

  return 0;
}
