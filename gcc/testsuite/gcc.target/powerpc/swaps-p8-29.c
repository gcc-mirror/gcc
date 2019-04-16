/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 " } */

#include <altivec.h>

extern void abort (void);

const vector char y = { 0, 1, 2, 3,
			4, 5, 6, 7,
			8, 9, 10, 11,
			12, 13, 14, 15 };

vector char x, z;

vector char
foo (void)
{
  return y;			/* Remove 1 swap and use lvx.  */
}

vector char
foo1 (void)
{
  x = y;			/* Remove 2 redundant swaps here.  */
  return x;			/* Remove 1 swap and use lvx.  */
}

void __attribute__ ((noinline))
fill_local (vector char *vp)
{
  *vp = x;			/* Remove 2 redundant swaps here.  */
}

/* Test aligned load from local.  */
vector char
foo2 (void)
{
  vector char v;

  /* Need to be clever here because v will normally reside in a
     register rather than memory.  */
  fill_local (&v);
  return v;			/* Remove 1 swap and use lvx.  */
}


/* Test aligned load from pointer.  */
vector char
foo3 (vector char *arg)
{
  return *arg;			/* Remove 1 swap and use lvx.  */
}

/* In this structure, the compiler should insert padding to assure
   that a_vector is properly aligned.  */
struct bar {
  char a_field;
  vector char a_vector;
};

vector char
foo4 (struct bar *bp)
{
  return bp->a_vector;		/* Remove 1 swap and use lvx.  */
}

/* Test aligned store to global.  */
void
baz (vector char arg)
{
  x = arg;			/* Remove 1 swap and use stvx.  */
}

void __attribute__ ((noinline))
copy_local (vector char *arg)
{
  x = *arg;			/* Remove 2 redundant swaps.  */
}


/* Test aligned store to local.  */
void
baz1 (vector char arg)
{
  vector char v;

  /* Need cleverness, because v will normally reside in a register
     rather than memory.  */
  v = arg;			/* Aligned store to local: remove 1
				   swap and use stvx.  */
  copy_local (&v);
}

/* Test aligned store to pointer.  */
void
baz2 (vector char *arg1, vector char arg2)
{
  /* Assume arg2 resides in register.  */
  *arg1 = arg2;			/* Remove 1 swap and use stvx.  */
}

void
baz3 (struct bar *bp, vector char v)
{
  /* Assume v resides in register.  */
  bp->a_vector = v;		/* Remove 1 swap and use stvx.  */
}

int
main (int argc, char *argv[])
{
  vector char fetched_value = foo ();
  if (fetched_value[0] != 0 || fetched_value[15] != 15)
    abort ();

  fetched_value = foo1 ();
  if (fetched_value[1] != 1 || fetched_value[14] != 14)
    abort ();

  fetched_value = foo2 ();
  if (fetched_value[2] != 2 || fetched_value[13] != 13)
    abort ();

  fetched_value = foo3 (&x);
  if (fetched_value[3] != 3 || fetched_value[12] != 12)
    abort ();

  struct bar a_struct;
  a_struct.a_vector = x;	/* Remove 2 redundant swaps.  */
  fetched_value = foo4 (&a_struct);
  if (fetched_value[4] != 4 || fetched_value[11] != 11)
    abort ();

  for (int i = 0; i < 16; i++)
    z[i] = 15 - i;

  baz (z);
  if (x[0] != 15 || x[15] != 0)
    abort ();

  vector char source = { 8, 7, 6, 5, 4, 3, 2, 1,
			 0, 9, 10, 11, 12, 13, 14, 15 };

  baz1 (source);
  if (x[3] != 5 || x[8] != 0)
    abort ();

  vector char dest;
  baz2 (&dest, source);
  if (dest[4] != 4 || dest[1] != 7)
    abort ();

  baz3 (&a_struct, source);
  if (a_struct.a_vector[7] != 1 || a_struct.a_vector[15] != 15)
    abort ();

  return 0;
}
