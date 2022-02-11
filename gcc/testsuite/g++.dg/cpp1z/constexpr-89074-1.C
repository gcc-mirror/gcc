// PR c++/89074
// { dg-do compile { target c++17 } }

struct S { int s; };
struct T : public S { };
struct U : public T { };

constexpr bool
foo ()
{
  U a[] = { 1, 2, 3, 4 };
  U b[] = { 5, 6, 7, 8 };
  T *c = (T *) a + 1;
  S *d = (S *) c + 2;
  S *e = (S *) b + 1;

  if (a + 0 == b + 0)
    return false;

  if (d == e)
    return false;

  return true;
}

static_assert (foo (), "");
