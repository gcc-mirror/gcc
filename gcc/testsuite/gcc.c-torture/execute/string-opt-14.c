/* Copyright (C) 2001  Free Software Foundation.

   Ensure builtin memset and memcpy are optimized away correctly.

   Written by Roger Sayle, 11/23/2001.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memset (void *s, int c, size_t n);
extern void *memcpy (void *dest, const void *src, size_t n);

char dst[32];
char src[32];

int
main ()
{
    memset (src, 0, 0);
    memcpy (dst, src, 0);
    return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */

__attribute__ ((noinline))
static void *
memset (void *s, int c, size_t n)
{
  abort ();
}

__attribute__ ((noinline))
static void *
memcpy (void *dest, const void *src, size_t n)
{
  abort ();
}
#endif

