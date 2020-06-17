/* { dg-additional-options "-fanalyzer-transitivity" } */

#include <stddef.h>
#include <alloca.h>
#include <stdlib.h>

extern void do_stuff (const void *);

#define LIMIT 1024

void test_1 (size_t sz)
{
  void *ptr;
  if (sz >= LIMIT)
    ptr = malloc (sz);
  else
    ptr = alloca (sz);

  do_stuff (ptr);

  if (sz >= LIMIT)
    free (ptr);
}

void test_2 (size_t sz)
{
  void *ptr;
  if (sz < LIMIT)
    ptr = alloca (sz);
  else
    ptr = malloc (sz);

  do_stuff (ptr);

  if (sz >= LIMIT)
    free (ptr);
}

void test_3 (size_t sz)
{
  void *ptr;
  if (sz <= LIMIT)
    ptr = alloca (sz); /* { dg-message "memory is allocated on the stack here" } */
  else
    ptr = malloc (sz);

  do_stuff (ptr);

  /* Bug: the "sz <= LIMIT" above should have been "sz < LIMIT",
     so there's a free-of-alloca when sz == LIMIT.  */
  if (sz >= LIMIT)
    free (ptr); /* { dg-warning "'free' of memory allocated on the stack by 'alloca'" } */
}
/* { dg-bogus "leak of 'ptr'" } */
/* This can't happen, as "sz > 1024" && "sz <= 1023" is impossible.  */
