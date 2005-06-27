/* { dg-do compile { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern char buf[0x40000000];

void
test1 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
  if (__builtin_object_size (p, 0) != sizeof (buf) - 8)
    abort ();
}

void
test2 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
  if (__builtin_object_size (p, 1) != sizeof (buf) - 8)
    abort ();
}

void
test3 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
  if (__builtin_object_size (p, 2) != 0)
    abort ();
}

void
test4 (size_t x)
{
  char *p = &buf[8];
  size_t i;

  for (i = 0; i < x; ++i)
    p = p + 4;
  if (__builtin_object_size (p, 3) != 0)
    abort ();
}

/* { dg-final { scan-assembler-not "abort" } } */
