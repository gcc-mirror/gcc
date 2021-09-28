/* { dg-options "-g" } */
/* { dg-do run { xfail { ! aarch64*-*-* } } } */
/* { dg-xfail-run-if "" aarch64*-*-* "*" { "-O[01g]" } } */

#define GUALITY_DONT_FORCE_LIVE_AFTER -1

#ifndef STATIC_INLINE
#define STATIC_INLINE /*static*/
#endif

#include "guality.h"

#include <assert.h>

/* Test the debug info for the functions used in the VTA
   presentation at the GCC Summit 2008.  */

typedef struct list {
  struct list *n;
  int v;
} elt, *node;

STATIC_INLINE node
find_val (node c, int v, node e)
{
  while (c < e)
    {
      GUALCHK (c);
      GUALCHK (v);
      GUALCHK (e);
      if (c->v == v)
	return c;
      GUALCHK (c);
      GUALCHK (v);
      GUALCHK (e);
      c++;
    }
  return NULL;
}

STATIC_INLINE node
find_prev (node c, node w)
{
  while (c)
    {
      node o = c;
      c = c->n;
      GUALCHK (c);
      GUALCHK (o);
      GUALCHK (w);
      if (c == w)
	return o;
      GUALCHK (c);
      GUALCHK (o);
      GUALCHK (w);
    }
  return NULL;
}

STATIC_INLINE node
check_arr (node c, node e)
{
  if (c == e)
    return NULL;
  e--;
  while (c < e)
    {
      GUALCHK (c);
      GUALCHK (e);
      if (c->v > (c+1)->v)
	return c;
      GUALCHK (c);
      GUALCHK (e);
      c++;
    }
  return NULL;
}

STATIC_INLINE node
check_list (node c, node t)
{
  while (c != t)
    {
      node n = c->n;
      GUALCHK (c);
      GUALCHK (n);
      GUALCHK (t);
      if (c->v > n->v)
	return c;
      GUALCHK (c);
      GUALCHK (n);
      GUALCHK (t);
      c = n;
    }
  return NULL;
}

struct list testme[] = {
  { &testme[1],  2 },
  { &testme[2],  3 },
  { &testme[3],  5 },
  { &testme[4],  7 },
  { &testme[5], 11 },
  { NULL, 13 },
};

int
main (int argc, char *argv[])
{
  int n = sizeof (testme) / sizeof (*testme);
  node first, last, begin, end, ret;

  GUALCHKXPR (n);

  begin = first = &testme[0];
  last = &testme[n-1];
  end = &testme[n];

  GUALCHKXPR (first);
  GUALCHKXPR (last);
  GUALCHKXPR (begin);
  GUALCHKXPR (end);

  ret = find_val (begin, 13, end);
  GUALCHK (ret);
  assert (ret == last);

  ret = find_prev (first, last);
  GUALCHK (ret);
  assert (ret == &testme[n-2]);

  ret = check_arr (begin, end);
  GUALCHK (ret);
  assert (!ret);

  ret = check_list (first, last);
  GUALCHK (ret);
  assert (!ret);
}
