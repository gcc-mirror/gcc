/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized -fno-ipa-vrp"  } */

class A
{
public:
  __attribute__ ((noinline))
  int Foo2()
  {
    return 1;
  }

  int v;
  float f;
};

class B
{
public:
  __attribute__ ((noinline))
  int Bar2()
  {
    return 1;
  }

  int v;
  float f, aaa;
};

int main()
{
  A a;
  B b;

  return a.Foo2() + b.Bar2();
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
