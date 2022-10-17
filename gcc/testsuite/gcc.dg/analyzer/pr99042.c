#include <stdio.h>

struct foo {
  FILE *file;
};

extern void unknown_fn ();
extern void unknown_fn2 (const struct foo *f);

int test_1 (struct foo *p)
{
  if ((p->file = fopen("test.txt", "w")) == NULL)
    return 1;
  unknown_fn ();
  return 0; /* { dg-bogus "leak" } */
}

int test_2 (struct foo *p)
{
  if ((p->file = fopen("test.txt", "w")) == NULL)
    return 1;
  return 0; /* { dg-bogus "leak" } */
}

int test_3 (void)
{
  struct foo f;
  struct foo *p = &f;
  if ((p->file = fopen("test.txt", "w")) == NULL)
    return 1;
  unknown_fn ();
  return 0;
} /* { dg-warning "leak" } */

int test_4 (void)
{
  struct foo f;
  struct foo *p = &f;
  if ((p->file = fopen("test.txt", "w")) == NULL)
    return 1;
  return 0;
} /* { dg-warning "leak" } */

int test_5 (void)
{
  struct foo f;
  struct foo *p = &f;
  if ((p->file = fopen("test.txt", "w")) == NULL)
    return 1;
  /* Although p is const, the underlying FILE * is not and could be closed.  */
  unknown_fn2 (p);
  return 0; /* { dg-bogus "leak" } */
}
