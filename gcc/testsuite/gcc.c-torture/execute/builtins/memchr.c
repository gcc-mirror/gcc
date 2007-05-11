/* Copyright (C) 2007  Free Software Foundation.

   Ensure all expected transformations of builtin memchr occur
   and perform correctly.

   Written by Paolo Carlini, 10/5/2007.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memchr (const void *, int, size_t);

void
main_test (void)
{
  const char* const foo1 = "hello world";

  if (memchr (foo1, 'x', 11))
    abort ();
  if (memchr (foo1, 'o', 11) != foo1 + 4)
    abort ();
  if (memchr (foo1, 'w', 2))
    abort ();
  if (memchr (foo1 + 5, 'o', 6) != foo1 + 7)
    abort ();
  if (memchr (foo1, 'd', 11) != foo1 + 10)
    abort ();
  if (memchr (foo1, 'd', 10))
    abort ();
  if (memchr (foo1, '\0', 11))
    abort ();
  if (memchr (foo1, '\0', 12) != foo1 + 11)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_memchr (foo1, 'r', 11) != foo1 + 8)
    abort ();
}
