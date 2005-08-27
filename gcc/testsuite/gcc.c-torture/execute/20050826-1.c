/* PR rtl-optimization/23561 */

struct A
{
  char a1[1];
  char a2[5];
  char a3[1];
  char a4[2048 - 7];
} a;

typedef __SIZE_TYPE__ size_t;
extern void *memset (void *, int, size_t);
extern void *memcpy (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);
extern void abort (void);

void
bar (struct A *x)
{
  size_t i;
  if (memcmp (x, "\1HELLO\1", sizeof "\1HELLO\1"))
    abort ();
  for (i = 0; i < sizeof (x->a4); i++)
    if (x->a4[i])
      abort ();
}

int
foo (void)
{
  memset (&a, 0, sizeof (a));
  a.a1[0] = 1;
  memcpy (a.a2, "HELLO", sizeof "HELLO");
  a.a3[0] = 1;
  bar (&a);
  return 0;
}

int
main (void)
{
  foo ();
  return 0;
}
