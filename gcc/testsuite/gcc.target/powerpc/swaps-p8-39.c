/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 " } */
/* { dg-require-effective-target powerpc_vsx } */

/* Previous versions of this test required that the assembler does not
   contain xxpermdi or xxswapd.  However, with the more sophisticated
   code generation used today, it is now possible that xxpermdi (aka
   xxswapd) show up without being part of a lxvd2x or stxvd2x
   sequence.  */

#include <altivec.h>

extern void abort (void);

vector float x;
const vector float y = { 0.0F, 0.1F, 0.2F, 0.3F };
vector float z;

vector float
foo (void)
{
  return y;			/* Remove 1 swap and use lvx.  */
}

vector float
foo1 (void)
{
  x = y;			/* Remove 2 redundant swaps here.  */
  return x;			/* Remove 1 swap and use lvx.  */
}

void __attribute__ ((noinline))
fill_local (vector float *vp)
{
  *vp = x;			/* Remove 2 redundant swaps here.  */
}

/* Test aligned load from local.  */
vector float
foo2 (void)
{
  vector float v;

  /* Need to be clever here because v will normally reside in a
     register rather than memory.  */
  fill_local (&v);
  return v;			/* Remove 1 swap and use lvx.  */
}


/* Test aligned load from pointer.  */
vector float
foo3 (vector float *arg)
{
  return *arg;			/* Remove 1 swap and use lvx.  */
}

/* In this structure, the compiler should insert padding to assure
   that a_vector is properly aligned.  */
struct bar {
  short a_field;
  vector float a_vector;
};

vector float
foo4 (struct bar *bp)
{
  return bp->a_vector;		/* Remove 1 swap and use lvx.  */
}

/* Test aligned store to global.  */
void
baz (vector float arg)
{
  x = arg;			/* Remove 1 swap and use stvx.  */
}

void __attribute__ ((noinline))
copy_local (vector float *arg)
{
  x = *arg;			/* Remove 2 redundant swaps.  */
}


/* Test aligned store to local.  */
void
baz1 (vector float arg)
{
  vector float v;

  /* Need cleverness, because v will normally reside in a register
     rather than memory.  */
  v = arg;			/* Aligned store to local: remove 1
				   swap and use stvx.  */
  copy_local (&v);
}

/* Test aligned store to pointer.  */
void
baz2 (vector float *arg1, vector float arg2)
{
  /* Assume arg2 resides in register.  */
  *arg1 = arg2;			/* Remove 1 swap and use stvx.  */
}

void
baz3 (struct bar *bp, vector float v)
{
  /* Assume v resides in register.  */
  bp->a_vector = v;		/* Remove 1 swap and use stvx.  */
}

int
main (float argc, float *argv[])
{
  vector float fetched_value = foo ();
  if (fetched_value[0] != 0.0F || fetched_value[3] != 0.3F)
    abort ();

  fetched_value = foo1 ();
  if (fetched_value[1] != 0.1F || fetched_value[2] != 0.2F)
    abort ();

  fetched_value = foo2 ();
  if (fetched_value[2] != 0.2F || fetched_value[1] != 0.1F)
    abort ();

  fetched_value = foo3 (&x);
  if (fetched_value[3] != 0.3F || fetched_value[0] != 0.0F)
    abort ();

  struct bar a_struct;
  a_struct.a_vector = x;	/* Remove 2 redundant swaps.  */
  fetched_value = foo4 (&a_struct);
  if (fetched_value[2] != 0.2F || fetched_value[3] != 0.3F)
    abort ();

  z[0] = 0.7F;
  z[1] = 0.6F;
  z[2] = 0.5F;
  z[3] = 0.4F;

  baz (z);
  if (x[0] != 0.7F || x[3] != 0.4F)
    abort ();

  vector float source = { 0.8F, 0.7F, 0.6F, 0.5F };

  baz1 (source);
  if (x[3] != 0.6F || x[2] != 0.7F)
    abort ();

  vector float dest;
  baz2 (&dest, source);
  if (dest[0] != 0.8F || dest[1] != 0.7F)
    abort ();

  baz3 (&a_struct, source);
  if (a_struct.a_vector[3] != 0.5F || a_struct.a_vector[0] != 0.8F)
    abort ();

  return 0;
}
