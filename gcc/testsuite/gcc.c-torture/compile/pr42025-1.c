typedef void* Ptr;

struct A
{
  int i;
  union
  {
    Ptr p;
    char *q;
  } u;
};

static void foo(struct A *p, char *q)
{
  if (p->i)
    p->u.p = 0;
  else
    p->u.q = q;
}

void bar(struct A *p, char *q)
{
  foo(p, q);
}
