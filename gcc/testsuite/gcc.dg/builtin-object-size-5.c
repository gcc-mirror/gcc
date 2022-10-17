/* { dg-do compile { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } */
/* { dg-options "-O2" } */

#ifndef N
# define N 0x40000000
#endif

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern char buf[N];

void
test1 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 0) != sizeof (buf) - 8 - 4 * x)
#else
  if (__builtin_object_size (p, 0) != sizeof (buf) - 8)
#endif
    abort ();
}

void
test2 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 1) != sizeof (buf) - 8 - 4 * x)
#else
  if (__builtin_object_size (p, 1) != sizeof (buf) - 8)
#endif
    abort ();
}

void
test3 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 2) != sizeof (buf) - 8 - 4 * x)
#else
  if (__builtin_object_size (p, 2) != 0)
#endif
    abort ();
}

void
test4 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
#ifdef __builtin_object_size
  if (__builtin_object_size (p, 3) != sizeof (buf) - 8 - 4 * x)
#else
  if (__builtin_object_size (p, 3) != 0)
#endif
    abort ();
}

void
test5 (void)
{
  char *p = &buf[0x90000004];
  if (__builtin_object_size (p + 2, 0) != 0)
    abort ();
}

void
test6 (void)
{
  char *p = &buf[-4];
  if (__builtin_object_size (p + 2, 0) != 0)
    abort ();
}

#ifdef __builtin_object_size
void
test7 (void)
{
  char *buf2 = __builtin_malloc (8);
  char *p = &buf2[0x90000004];
  if (__builtin_object_size (p + 2, 0) != 0)
    abort ();
}
#endif

/* { dg-final { scan-assembler-not "abort" } } */
