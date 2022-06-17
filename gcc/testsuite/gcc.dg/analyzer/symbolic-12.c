#include "analyzer-decls.h"

void external_fn(void);

struct st_1
{
  char *name;
  unsigned size;
};

void test_1a (void *p, unsigned next_off)
{
  struct st_1 *r = p;

  external_fn();

  if (next_off >= r->size)
    return;

  if (next_off >= r->size)
    /* We should have already returned if this is the case.  */
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

void test_1b (void *p, unsigned next_off)
{
  struct st_1 *r = p;

  if (next_off >= r->size)
    return;

  if (next_off >= r->size)
    /* We should have already returned if this is the case.  */
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

void test_1c (struct st_1 *r, unsigned next_off)
{
  if (next_off >= r->size)
    return;

  if (next_off >= r->size)
    /* We should have already returned if this is the case.  */
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

void test_1d (struct st_1 *r, unsigned next_off)
{
  external_fn();

  if (next_off >= r->size)
    return;

  if (next_off >= r->size)
    /* We should have already returned if this is the case.  */
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

void test_1e (void *p, unsigned next_off)
{
  struct st_1 *r = p;

  while (1)
    {
      external_fn();

      if (next_off >= r->size)
	return;

      __analyzer_dump_path (); /* { dg-message "path" } */
    }
}

struct st_2
{
  char *name;
  unsigned arr[10];
};

void test_2a (void *p, unsigned next_off)
{
  struct st_2 *r = p;

  external_fn();

  if (next_off >= r->arr[5])
    return;

  if (next_off >= r->arr[5])
    /* We should have already returned if this is the case.  */
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

void test_2b (void *p, unsigned next_off, int idx)
{
  struct st_2 *r = p;

  external_fn();

  if (next_off >= r->arr[idx])
    return;

  if (next_off >= r->arr[idx])
    /* We should have already returned if this is the case.  */
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}
