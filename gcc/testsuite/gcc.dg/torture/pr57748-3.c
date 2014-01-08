/* PR middle-end/57748 */
/* { dg-do run } */
/* wrong code in expand_expr_real_1.  */

#include <stdlib.h>

extern void abort (void);

typedef long long V
  __attribute__ ((vector_size (2 * sizeof (long long)), may_alias));

typedef struct S { V a; V b[0]; } P __attribute__((aligned (1)));

struct __attribute__((packed)) T { char c; P s; };

void __attribute__((noinline, noclone))
check (P *p)
{
  if (p->b[0][0] != 3 || p->b[0][1] != 4)
    abort ();
}

void __attribute__((noinline, noclone))
foo (struct T *t)
{
  V a = { 3, 4 };
  t->s.b[0] = a;
}

int
main ()
{
  struct T *t = (struct T *) calloc (128, 1);

  foo (t);
  check (&t->s);

  free (t);
  return 0;
}
