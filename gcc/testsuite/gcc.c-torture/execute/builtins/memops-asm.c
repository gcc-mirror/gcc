/* Copyright (C) 2003 Free Software Foundation.

   Test memcpy and memset in presence of redirect.  */

#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern void *memcpy (void *, const void *, size_t)
  __asm (ASMNAME ("my_memcpy"));
extern void bcopy (const void *, void *, size_t)
  __asm (ASMNAME ("my_bcopy"));
extern void *memmove (void *, const void *, size_t)
  __asm (ASMNAME ("my_memmove"));
extern void *memset (void *, int, size_t)
  __asm (ASMNAME ("my_memset"));
extern void bzero (void *, size_t)
  __asm (ASMNAME ("my_bzero"));
extern int memcmp (const void *, const void *, size_t);

struct A { char c[32]; } a = { "foobar" };
char x[64] = "foobar", y[64];
int i = 39, j = 6, k = 4;

extern int inside_main;

void
main_test (void)
{
  struct A b = a;
  struct A c = { { 'x' } };

  inside_main = 1;
  
  if (memcmp (b.c, x, 32) || c.c[0] != 'x' || memcmp (c.c + 1, x + 32, 31))
    abort ();
  if (__builtin_memcpy (y, x, i) != y || memcmp (x, y, 64))
    abort ();
  if (memcpy (y + 6, x, j) != y + 6
      || memcmp (x, y, 6) || memcmp (x, y + 6, 58))
    abort ();
  if (__builtin_memset (y + 2, 'X', k) != y + 2
      || memcmp (y, "foXXXXfoobar", 13))
    abort ();
  bcopy (y + 1, y + 2, 6);
  if (memcmp (y, "fooXXXXfobar", 13))
    abort ();
  __builtin_bzero (y + 4, 2);
  if (memcmp (y, "fooX\0\0Xfobar", 13))
    abort ();
}
