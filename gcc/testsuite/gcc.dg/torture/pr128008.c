/* { dg-do run } */

struct T { int v; struct T *restrict next; };

__attribute__((noipa)) void
f(struct T *restrict p)
{
  p->next->v = 7;
}

int main()
{
  struct T *q = __builtin_malloc(sizeof *q);
  q->next = __builtin_malloc(sizeof *q);
  q->next->v = 2;
  f(q);
  if (q->next->v != 7)
    __builtin_abort();
  return 0;
}
