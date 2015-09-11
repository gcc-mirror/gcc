/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

class A
{
public:
  __attribute__ ((noinline))
  virtual int Foo2()
  {
    return v;
  }

  float f;
  int v;
};

class B
{
public:
  __attribute__ ((noinline))
  int Bar2()
  {
    return v;
  }

  float f, aaa;
  int v;
};

int main()
{
  A a;
  B b;

  a.Foo2();
  b.Bar2();

  return 12345;
}

/* { dg-final { scan-ipa-dump-not "Semantic equality hit:" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
