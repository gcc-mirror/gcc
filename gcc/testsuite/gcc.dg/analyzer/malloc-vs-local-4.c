#include <stdlib.h>

void __attribute__((noinline)) callee_1 (int *ptr)
{
  *ptr = 42; /* { dg-warning "dereference of possibly-NULL 'ptr'" } */
}

int test_1 (int i, int flag)
{
  /* Double diamond CFG; either use &i, or a malloc-ed buffer.  */
  int *ptr = &i;
  if (flag)
    ptr = (int *)malloc (sizeof (int));
  callee_1 (ptr);
  if (flag)
    free (ptr);
  return i;
}

void __attribute__((noinline)) callee_2 (int *ptr)
{
  *ptr = 42;
}

int test_2 (int flag)
{
  int i;

  if (flag)
    callee_2 (&i);

  callee_2 (&i);

  if (!flag)
    {
      void *ptr = malloc (16);
      free (ptr);
      free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
    }
}
