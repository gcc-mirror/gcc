// { dg-do run  }
// extern "C" printf(const char *, ...);

class X
{
public:
  int  a;
  int f(int);
};

class Y
{
public:
  int b;
  int c;
  int g(int);
};

class MD : public X, public Y
{
public:
  int c;
  int hf(int);
};

int MD::* pmi0 = &MD::a;
int MD::* pmi1 = &MD::b;
int MD::* pmi2 = &MD::c;

int (MD::* pmf0)(int) = &MD::f;
int (MD::* pmf1)(int) = &MD::g;
int (MD::* pmf2)(int) = &MD::hf;

int main()
{
  MD obj;
  int fail = 0;

  obj.a = 1;
  obj.b = 2;
  obj.c = 3;

  obj.*pmi0 = 7;
  obj.*pmi1 = 8;
  obj.*pmi2 = 9;

  fail += (obj.*pmf0)(7);
  fail += (obj.*pmf1)(8);
  fail += (obj.*pmf2)(9);

#if 0
  if (fail != 0)
    printf ("failed %d tests\n", fail);
  else
    printf ("passed\n");

  printf ("sizeof(X) = %d, sizeof(Y) = %d, sizeof(MD) = %d\n",
	  sizeof(X), sizeof(Y), sizeof(MD));
#endif
  return fail;
}

int X::f(int v)
{
  if (v != a)
  {
//    printf ("failed in X::f, a = %d\n", a);
    return 1;
  }
  return 0;
}

int Y::g(int v)
{
  if (v != b)
  {
//    printf ("failed in Y::g, b = %d\n", b);
    return 1;
  }
  return 0;
}

int MD::hf(int v)
{
  if (v != c)
  {
//    printf ("failed in MD::hf, c = %d\n", c);
    return 1;
  }
  return 0;
}
