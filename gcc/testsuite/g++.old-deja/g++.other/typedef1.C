// { dg-do assemble  }

typedef const struct {
   int x;
} Test;

static void foo(Test);

static void foo(Test t)
{
  t.x = 0; // { dg-error "" } assignment of read-only member
  return;
}
