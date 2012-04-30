/* This file was miscompiled by an earlier version of the object size
   checking patch.  Object size in one of the memcpy calls was
   incorrectly determined to be 0 while it should be (size_t) -1
   (== unknown).  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftrack-macro-expansion=0" } */

#include "../gcc.c-torture/execute/builtins/chk.h"
   
void *bar (int);
extern void *malloc (__SIZE_TYPE__);

struct A
{
  int i, j, k;
};

/* Here all object sizes are not known at compile time.  There
   should be no warning, nor any checker functions called.  */

void
foo (const struct A *x, int y, const unsigned char *z)
{
  unsigned int b;
  unsigned char *c = 0;

  b = (x->i & 0xff) == 1 ? 3 : 4;
  if (y)
    c = bar (x->j * x->k);

  const unsigned char *d = z;
  unsigned char *e = c;
  unsigned char *f = c + x->j * x->k;
  int g = 0;

  while (e < f)
    {
      unsigned int h = *d++;

      if (h & 128)
	{
	  h = h - 128;
	  g = e + h * b > f;
	  if (g)
	    h = (f - e) / b;
	  if (b < 4)
	    do
	      {
		memcpy (e, d, 3);
		e += 3;
	      }
	    while (--h);
	  else
	    do
	      {
		memcpy (e, d, 4);
		e += 4;
	      }
	    while (--h);
	  d += b;
	}
      else
	{
	  h *= b;
	  g = e + h > f;
	  if (g)
	    h = f - e;
	  memcpy (e, d, h);
	  e += h;
	  d += h;
	}
    }
}

/* The same routine, slightly modified:
   1) c has known size at compile time
   2) e += h was changed into e += 16.
      GCC could actually through VRP determine that
      in e += h is (h >= 0 && h <= 127), thus know
      it is pointer addition and not subtraction and
      know e's __builtin_object_size (e, 0) is at 512,
      but we are not there yet.  */

unsigned char *
baz (const struct A *x, const unsigned char *z)
{
  unsigned int b;
  unsigned char *c = 0;

  b = (x->i & 0xff) == 1 ? 3 : 4;
  c = malloc (512);

  const unsigned char *d = z;
  unsigned char *e = c;
  unsigned char *f = c + x->j * x->k;
  int g = 0;

  while (e < f)
    {
      unsigned int h = *d++;

      if (h & 128)
	{
	  h = h - 128;
	  g = e + h * b > f;
	  if (g)
	    h = (f - e) / b;
	  if (b < 4)
	    do
	      {
		memcpy (e, d, 3);
		e += 3;
	      }
	    while (--h);
	  else
	    do
	      {
		memcpy (e, d, 513); /* { dg-warning "will always overflow" "memcpy" } */
		e += 4;
	      }
	    while (--h);
	  d += b;
	}
      else
	{
	  h *= b;
	  g = e + h > f;
	  if (g)
	    h = f - e;
	  memcpy (e, d, h);
	  /* e += h; */
	  e += 16;
	  d += h;
	}
    }
  return c;
}
