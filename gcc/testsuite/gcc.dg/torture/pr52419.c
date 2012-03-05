/* PR middle-end/52419 */
/* { dg-do run } */

extern void abort (void);

typedef long long V
  __attribute__ ((vector_size (2 * sizeof (long long)), may_alias));

typedef struct S { V b; } P __attribute__((aligned (1)));

struct __attribute__((packed)) T { char c; P s; };

__attribute__((noinline, noclone)) void
foo (P *p)
{
  p->b[1] = 5;
}

int
main ()
{
  V a = { 3, 4 };
  struct T t;

  t.s.b = a;
  foo (&t.s);

  if (t.s.b[0] != 3 || t.s.b[1] != 5)
    abort ();

  return 0;
}
