// P0784R7
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct S { constexpr S () : s (5) {} constexpr S (int x) : s (x) {} int s; };

constexpr bool
foo ()
{
  int r = 0;
  S *p = new S ();
  p->s += 3;
  r += p->s;
  delete p;
  p = new S (12);
  p->s = p->s * 2;
  r += p->s;
  delete p;
  int *q = new int;
  *q = 25;
  r += *q;
  delete q;
  q = new int (1);
  r += *q;
  if (!q)
    return false;
  delete q;
  q = new int[5]{1,2,3,4,5};
  r += q[0] + q[4];
  delete[] q;
  q = new int[4];
  q[0] = 6;
  q[1] = 7;
  q[3] = 8;
  r += q[0] + q[1] + q[3];
  delete[] q;
  return r == 5 + 3 + 2 * 12 + 25 + 1 + 1 + 5 + 6 + 7 + 8;
}
constexpr bool a = foo ();
static_assert (a);
