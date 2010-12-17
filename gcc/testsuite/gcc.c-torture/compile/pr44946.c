struct A
{
  int i;
  long l;
};

struct B
{
  int i;
};

struct C
{
  int i;
  struct B b;
};

struct B foo (struct A a)
{
  struct C *c = (struct C *) &a;
  return c->b;
}
void bar (struct A a, struct B b)
{
  struct C *c = (struct C *) &a;
  c->b = b;
}
