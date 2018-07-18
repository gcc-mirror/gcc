/* PR middle-end/38454 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

extern inline __attribute__((gnu_inline, always_inline, artificial)) void *
memcpy (void *__restrict dest, const void *__restrict src, size_t len)
{
  return __builtin___memcpy_chk (dest, /* { dg-warning "writing" } */
				 src, len, __builtin_object_size (dest, 0));
}

struct S { char buf[10]; } s;

void
foo (void)
{
  char buf[12];
  char *p = buf + 4;
  struct S *q = (struct S *) p;
  memcpy (q, &s, sizeof (s));
}

/* { dg-final { scan-assembler "__memcpy_chk" } } */
