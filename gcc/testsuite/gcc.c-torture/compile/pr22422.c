/* We should not crash trying to figure out the points-to sets for the below.  We used to because we
   ended up adding pointers to the points-to set of the ANYTHING variable.  */
struct D
{
  int n;
  int c [8];
};

struct A
{
  int i;
  char *p;
};

struct B
{
  struct A *a;
  struct D *d;
};

int dtInsert1 (struct B *b)
{
  struct A a = { 0, 0 };
  struct D *d;
  b->a = &a;
  d = b->d;
  &d->c [d->n];
  return 0;
}

