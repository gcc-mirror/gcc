/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -g -fcheck-pointer-bounds -mmpx -fcompare-debug" } */

struct ts
{
  int field;
};

extern void test1 ();
extern void test2 (struct ts *);

static void
init (struct ts *c)
{
  c->field = -1;
}

struct ts
test3 (const struct ts *other)
{
  struct ts r;
  if (other->field != 0)
    test1 ();
  init (&r);
  test2 (&r);
  return r;
}
