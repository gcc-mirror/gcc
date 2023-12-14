#include "analyzer-decls.h"

extern int rand (void);

void test_1 (void)
{
  int   ret = 0;
  while (ret != 42)
    ret = rand() % 1000;

  if (ret != 42)
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

static void empty_local_fn (void) {}
extern void external_fn (void);

void test_2 (void)
{
  void (*callback) () = empty_local_fn;
  int   ret = 0;
  while (ret != 42)
    ret = rand() % 1000;

  (*callback) ();

  if (ret != 42)
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}

void test_3 (void)
{
  void (*callback) () = external_fn;
  int   ret = 0;
  while (ret != 42)
    ret = rand() % 1000;

  (*callback) ();

  if (ret != 42)
    __analyzer_dump_path (); /* { dg-bogus "path" } */
}
