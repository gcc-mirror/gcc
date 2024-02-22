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

vector short x;
const vector short y = { 0, 1, 2, 3, 4, 5, 6, 7 };
vector short z;

vector short
foo (void)
{
  return y;			/* Remove 1 swap and use lvx.  */
}

vector short
foo1 (void)
{
  x = y;			/* Remove 2 redundant swaps here.  */
  return x;			/* Remove 1 swap and use lvx.  */
}

void __attribute__ ((noinline))
fill_local (vector short *vp)
{
  *vp = x;			/* Remove 2 redundant swaps here.  */
}

/* Test aligned load from local.  */
vector short
foo2 (void)
{
  vector short v;

  /* Need to be clever here because v will normally reside in a
     register rather than memory.  */
  fill_local (&v);
  return v;			/* Remove 1 swap and use lvx.  */
}


/* Test aligned load from pointer.  */
vector short
foo3 (vector short *arg)
{
  return *arg;			/* Remove 1 swap and use lvx.  */
}

/* In this structure, the compiler should insert padding to assure
   that a_vector is properly aligned.  */
struct bar {
  short a_field;
  vector short a_vector;
};

vector short
foo4 (struct bar *bp)
{
  return bp->a_vector;		/* Remove 1 swap and use lvx.  */
}

/* Test aligned store to global.  */
void
baz (vector short arg)
{
  x = arg;			/* Remove 1 swap and use stvx.  */
}

void __attribute__ ((noinline))
copy_local (vector short *arg)
{
  x = *arg;			/* Remove 2 redundant swaps.  */
}


/* Test aligned store to local.  */
void
baz1 (vector short arg)
{
  vector short v;

  /* Need cleverness, because v will normally reside in a register
     rather than memory.  */
  v = arg;			/* Aligned store to local: remove 1
				   swap and use stvx.  */
  copy_local (&v);
}

/* Test aligned store to pointer.  */
void
baz2 (vector short *arg1, vector short arg2)
{
  /* Assume arg2 resides in register.  */
  *arg1 = arg2;			/* Remove 1 swap and use stvx.  */
}

void
baz3 (struct bar *bp, vector short v)
{
  /* Assume v resides in register.  */
  bp->a_vector = v;		/* Remove 1 swap and use stvx.  */
}

int
main (int argc, short *argv[])
{
  vector short fetched_value = foo ();
  if (fetched_value[0] != 0 || fetched_value[7] != 7)
    abort ();

  fetched_value = foo1 ();
  if (fetched_value[1] != 1 || fetched_value[6] != 6)
    abort ();

  fetched_value = foo2 ();
  if (fetched_value[2] != 2 || fetched_value[5] != 5)
    abort ();

  fetched_value = foo3 (&x);
  if (fetched_value[3] != 3 || fetched_value[4] != 4)
    abort ();

  struct bar a_struct;
  a_struct.a_vector = x;	/* Remove 2 redundant swaps.  */
  fetched_value = foo4 (&a_struct);
  if (fetched_value[4] != 4 || fetched_value[3] != 3)
    abort ();

  for (int i = 0; i < 8; i++)
    z[i] = 7 - i;

  baz (z);
  if (x[0] != 7 || x[7] != 0)
    abort ();

  vector short source = { 8, 7, 6, 5, 4, 3, 2, 1 };

  baz1 (source);
  if (x[3] != 5 || x[7] != 1)
    abort ();

  vector short dest;
  baz2 (&dest, source);
  if (dest[4] != 4 || dest[1] != 7)
    abort ();

  baz3 (&a_struct, source);
  if (a_struct.a_vector[7] != 1 || a_struct.a_vector[5] != 3)
    abort ();

  return 0;
}
