#include "analyzer-decls.h"
#define NULL ((void *)0)

void *x, *y;

void test_1 (int flag)
{
  void *p = __builtin_malloc (1024);
  if (flag)
    x = p;
  else
    y = p;
} /* { dg-bogus "leak" } */

struct s2
{
  void *f1;
  void *f2;
};

struct s2 test_2 (int flag)
{
  struct s2 r;
  r.f1 = NULL;
  r.f2 = NULL;
  void *p = __builtin_malloc (1024);
  if (flag)
    r.f1 = p;
  else
    r.f2 = p;
  return r;
}
