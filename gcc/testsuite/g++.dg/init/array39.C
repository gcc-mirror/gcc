// PR c++/65154
// { dg-do run { target c++11 } }

int cnt1 = 0,
    cnt2 = 0;

struct S_empty
{
    S_empty () {
	cnt1++;
    };
};

struct C1
{
  S_empty s;
};

struct S_init
{
  S_init () : i(42)
  {
    cnt2++;
  };
  int i;
};

struct C2
{
  S_init a, b;
};

int
main ()
{
  C1 c1[5]{};
  C2 c2[1]{};

  if (c2[0].a.i != 42 || c2[0].b.i != 42)
    return 1;

  if (cnt1 != 5 || cnt2 != 2)
    return 1;

  return 0;
}
