/* PR sanitizer/70712 */
/* { dg-do run } */

struct __attribute__((aligned (64))) S
{
  char s[4];
};

struct T
{
  char t[8];
  char u[480];

};

__attribute__((noinline, noclone)) void
foo (struct T *p, struct S *q)
{
  __builtin_memset (p->t, '\0', sizeof (p->t));
  __builtin_memset (p->u, '\0', sizeof (p->u));
  __builtin_memset (q->s, '\0', sizeof (q->s));
}

int
main ()
{
  struct S s;
  struct T t;
  foo (&t, &s);
  asm volatile ("" : : "r" (&t), "r" (&s) : "memory");
  return 0;
}
