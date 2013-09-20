/* PR middle-end/57748 */
/* { dg-do run } */
/* ICE in expand_assignment:
   misalignp == true, !MEM_P (to_rtx), offset != 0,
   => gcc_assert (TREE_CODE (offset) == INTEGER_CST) */

#include <stdlib.h>

extern void abort (void);

typedef long long V
  __attribute__ ((vector_size (2 * sizeof (long long)), may_alias));

typedef struct S { V a; V b[0]; } P __attribute__((aligned (1)));

struct __attribute__((packed)) T { char c; P s; };

void __attribute__((noinline, noclone))
check (struct T *t)
{
  if (t->s.b[0][0] != 3 || t->s.b[0][1] != 4)
    abort ();
}

int __attribute__((noinline, noclone))
get_i (void)
{
  return 0;
}

void __attribute__((noinline, noclone))
foo (P *p)
{
  V a = { 3, 4 };
  int i = get_i ();
  p->b[i] = a;
}

int
main ()
{
  struct T *t = (struct T *) calloc (128, 1);

  foo (&t->s);
  check (t);

  free (t);
  return 0;
}
