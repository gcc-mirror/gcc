/* { dg-do run } */
/* { dg-require-effective-target mempcpy } */
/* PR fortran/45636 */

typedef __SIZE_TYPE__ size_t;
void *memcpy (void *__restrict__, const void *__restrict__, size_t);
void *mempcpy (void *__restrict__, const void *__restrict__, size_t);
void *memset (void *, int, size_t);
int memcmp (const void *, const void *, size_t);
extern void abort (void);

struct A { int i; char c[32]; } a[2];

__attribute__((noinline, noclone)) int
f1 (char *p, int q, int z)
{
  memcpy (p, "abcd", 4);
  if (q)
    z = z + 123;
  else
    z *= 114;
  memset (p + 4, ' ', 2);
  return z;
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  char *p = mempcpy (&a[0].c[13], "123456", 4);
  memset (p, '7', 3);
}

__attribute__((noinline, noclone)) void
f3 (struct A *p)
{
  p++;
  char *q = &p->c[10];
  memcpy (q + 4, "__1234567" + 2, 7);
  memset (&p->c[21], '9', 3);
}

__attribute__((noinline, noclone)) void
f4 (void)
{
  memcpy (&a[0].c[10], "0123456789", 10);
  memset (&a[0].c[13], ' ', 3);
}

__attribute__((noinline, noclone)) void
check (const char *p, const char *str, size_t size)
{
  const char *q;
  for (q = (const char *) &a; q < p; q++)
    if (*q)
      abort ();
  if (memcmp (p, str, size) != 0)
    abort ();
  for (q = p + size; q < (const char *) (&a[0] + 2); q++)
    if (*q)
      abort ();
  memset (&a, '\0', sizeof a);
}

int
main (void)
{
  if (f1 (&a[0].c[7], 1, 2) != 125)
    abort ();
  check (&a[0].c[7], "abcd  ", 6);
  f2 ();
  check (&a[0].c[13], "1234777", 7);
  f3 (&a[0]);
  check (&a[1].c[14], "1234567999", 10);
  f4 ();
  check (&a[0].c[10], "012   6789", 10);
  return 0;
}
