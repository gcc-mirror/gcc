/* Copyright (C) 2002  Free Software Foundation.

   Ensure that builtin memset operations for constant length and
   non-constant assigned value don't cause compiler problems.

   Written by Roger Sayle, 21 April 2002.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memset (void *, int, size_t);

char buffer[32];

int
main (int argc)
{
  memset (buffer, argc, 0);
  memset (buffer, argc, 1);
  memset (buffer, argc, 2);
  memset (buffer, argc, 3);
  memset (buffer, argc, 4);
  memset (buffer, argc, 5);
  memset (buffer, argc, 6);
  memset (buffer, argc, 7);
  memset (buffer, argc, 8);
  memset (buffer, argc, 9);
  memset (buffer, argc, 10);
  memset (buffer, argc, 11);
  memset (buffer, argc, 12);
  memset (buffer, argc, 13);
  memset (buffer, argc, 14);
  memset (buffer, argc, 15);
  memset (buffer, argc, 16);
  memset (buffer, argc, 17);

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, most of the above cases should be transformed into
   something else.  So any remaining calls to the original function
   for short lengths should abort.  */
static void *
memset (void *dst, int c, size_t len)
{
  if (len < 2)
    abort ();
}
#endif

