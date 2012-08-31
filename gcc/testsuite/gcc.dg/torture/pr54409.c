/* { dg-do compile } */

int b;

struct S
{
  char *p;
  struct {
  } s;
  int a;
};

static _Bool
fn2 (int *p1)
{
  if (b)
    {
      struct S *c = (struct S *) &p1;
      return c->a;
    }
}

_Bool
fn3 (struct S *p1)
{
  if (fn2 ((int *) &p1->s))
    return 0;
}
