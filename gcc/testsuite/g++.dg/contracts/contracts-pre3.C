// tests to ensure pre contracts work on member functions
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

namespace member
{
  int x = 10;
  double y = 10.5;

  struct S
  {
    bool z;
  };

  struct T1
  {
    void vfun(int m, double n)
      [[ pre: x < 0 ]]
      [[ pre: m < 0 ]];

    int fun(int m, double n)
      [[ pre: x < 0 ]];

    double fun2(int m, double n)
      [[ pre: x < 0 ]]
      [[ pre: y < 0 ]]
      [[ pre: m < 0 ]]
      [[ pre: n < 0 ]];

    S funend(int m, double n)
      [[ pre: x < 0 ]]
      [[ pre: m < 0 ]];
  };

  void T1::vfun(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]]
  {
    printf("vfun::x: %d\n", x);
  }


  int T1::fun(int m, double n)
    [[ pre: x < 0 ]]
  {
    printf("fun::x: %d\n", x);
    return x;
  }

  double T1::fun2(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: y < 0 ]]
    [[ pre: m < 0 ]]
    [[ pre: n < 0 ]]
  {
    printf("fun2::x: %d fun2::y: %f\n", x, y);
    return y;
  }
}

namespace special
{
  struct T1
  {
    T1(int m, int n)
      [[ pre: m < 0 ]]
      [[ pre: n < 0 ]];

    int operator+(int m)
      [[ pre: m > 0 ]]
      [[ pre: v > 0 ]];

    int operator-(int m)
      [[ pre: m > 0 ]]
      [[ pre: v > 0 ]];

    ~T1()
      [[ pre: v > 0 ]];

    int v{-10};
  };

  T1::T1(int m, int n)
    [[ pre: m < 0 ]]
    [[ pre: n < 0 ]]
    : v{-m * n}
  {
  }

  int T1::operator+(int m)
    [[ pre: m > 0 ]]
    [[ pre: v > 0 ]]
  {
    return v + m;
  }

  int T1::operator-(int m)
    [[ pre: m > 0 ]]
    [[ pre: v > 0 ]]
  {
    return v - m;
  }

  T1::~T1()
    [[ pre: v > 0 ]]
  {
  }

  struct T2
  {
    T2(int m, int n)
      [[ pre: m < 0 ]]
      [[ pre: n < 0 ]]
      : v{-m * n}
    {
    }

    int operator+(int m)
      [[ pre: m > 0 ]]
      [[ pre: v > 0 ]]
    {
      return v + m;
    }

    int operator-(int m)
      [[ pre: m > 0 ]]
      [[ pre: v > 0 ]]
    {
      return v - m;
    }

    ~T2()
      [[ pre: v > 0 ]]
    {
    }

    int v{-10};
  };

  struct TC : T1
  {
    TC(int m, int n)
      [[ pre: m < -1 ]]
      [[ pre: n < -1 ]]
      : T1{m, n}
    {
    }

    ~TC()
      [[ pre: vc < 0 ]]
    {
    }

    TC(int a)
      [[ pre: a < 0 ]]
      : TC{a, a}
    {
    }

    int vc{10};
  };

  void test()
  {
    T1 t1{10, 20};
    int a = t1 - -5;
    int b = t1 + -5;
    printf("==========\n");

    T2 t2{10, 20};
    int k = t2 - -5;
    int j = t2 + -5;
    printf("==========\n");

    TC tc{10, 20};
    printf("==========\n");

    TC tc2{10};
    printf("==========\n");
  }
}

namespace virt
{
  struct T1
  {
    virtual int fun(int m, int n)
      [[ pre: m > 0 ]]
      [[ pre: n > 0 ]]
      [[ pre: v > 0 ]];
    int v{-10};
  };

  int T1::fun(int m, int n)
    [[ pre: m > 0 ]]
    [[ pre: n > 0 ]]
    [[ pre: v > 0 ]]
  {
    printf("T1::fun::m: %d, T1::fun::n: %d, T1::v: %d\n", m, n, v);
    return m * n * v;
  }

  struct T2 : T1
  {
  };

  struct T3 : T2
  {
    virtual int fun(int m, int n)
      [[ pre: m > 0 ]]
      [[ pre: n > 0 ]]
      [[ pre: v > 0 ]]
      override
    {
      printf("T3::fun::m: %d, T3::fun::n: %d, T3::v: %d\n", m, n, v);
      return m * n * v;
    }
  };

  struct T3b : T2
  {
    virtual int fun(int m, int n)
      [[ pre: m > 0 ]]
      [[ pre: n > 0 ]]
      [[ pre: v > 0 ]]
      override;

    int p(int a)
      [[ pre: a > 0 ]]
      [[ pre: v > 0 ]];

    int u(int a)
      [[ pre: a > 0 ]]
      [[ pre: z > 0 ]];

    int n(int a)
      [[ pre: a > 0 ]];

    static int Sn(int a)
      [[ pre: a > 0 ]];

    int z{-10};
  };

  int T3b::fun(int m, int n)
    [[ pre: m > 0 ]]
    [[ pre: n > 0 ]]
    [[ pre: v > 0 ]]
  {
    printf("T3b::fun::m: %d, T3b::fun::n: %d, T3b::v: %d\n", m, n, v);
    return m * n * v;
  }

  int T3b::p(int a)
    [[ pre: a > 0 ]]
    [[ pre: v > 0 ]]
  {
    printf("T3b::p: a: %d, v: %d\n", a, v);
    return a * v;
  }

  int T3b::u(int a)
    [[ pre: a > 0 ]]
    [[ pre: z > 0 ]]
  {
    printf("T3b::u: a: %d, z: %d\n", a, z);
    return a * z;
  }

  int T3b::n(int a)
    [[ pre: a > 0 ]]
  {
    printf("T3b::n: a: %d\n", a);
    return a;
  }

  int T3b::Sn(int a)
    [[ pre: a > 0 ]]
  {
    printf("T3b::Sn: a: %d\n", a);
    return a;
  }

  struct T3c : T2
  {
    int fun(int m, int n)
      [[ pre: m > 0 ]]
      [[ pre: n > 0 ]]
      [[ pre: v > 0 ]]
    {
      printf("T3c::fun::m: %d, T3c::fun::n: %d, T3c::v: %d\n", m, n, v);
      return m * n * v;
    }

    int p(int a)
      [[ pre: a > 0 ]]
      [[ pre: v > 0 ]]
    {
      printf("T3c::p: a: %d, v: %d\n", a, v);
      return a * v;
    }

    int u(int a)
      [[ pre: a > 0 ]]
      [[ pre: z > 0 ]]
    {
      printf("T3c::u: a: %d, z: %d\n", a, z);
      return a * z;
    }

    int n(int a)
      [[ pre: a > 0 ]]
    {
      printf("T3c::n: a: %d\n", a);
      return a;
    }

    static int Sn(int a)
      [[ pre: a > 0 ]]
    {
      printf("T3c::Sn: a: %d\n", a);
      return a;
    }

    int z{-10};
  };

  void t(const char *kind, T1 *t)
  {
    printf("=================\n%s:\n", kind);
    t->fun(-1, -2);
  }

  void test()
  {
    T1 t1;
    t1.fun(-10, -20);

    T2 t2;
    t2.fun(-10, -20);

    T3 t3;
    t3.fun(-10, -20);

    T3b t3b;
    t3b.fun(-10, -20);

    T3c t3c;
    t3c.fun(-10, -20);

    t("T1", &t1);
    t("T2", &t2);
    t("T3", &t3);
    t("T3b", &t3b);
    t("T3c", &t3c);

    printf("=============\n");
    t3b.p(-3);
    t3b.u(-3);
    t3b.n(-3);
    T3b::Sn(-3);

    printf("=============\n");
    t3c.p(-3);
    t3c.u(-3);
    t3c.n(-3);
    T3c::Sn(-3);
  }
}

int main(int, char**) 
{
  // ordinary member functions
  {
    int x = 11;
    double y = 11.5;
    member::T1 t1;
    t1.vfun(x, y);

    int f = 13;
    f = t1.fun(x, y);
    printf("main::f: %d\n", f);
    double d = 13.37;
    d = t1.fun2(x, y);
    printf("main::d: %f\n", d);
    member::S s = t1.funend(x, y);
    printf("main::s.z: %d\n", s.z ? 1 : 0);
  }

  special::test();
  virt::test();
  return 0;
}

member::S member::T1::funend(int m, double n)
  [[ pre: x < 0 ]]
  [[ pre: m < 0 ]]
{
  printf("funend::x: %d\n", x);
  S s;
  s.z = true;
  return s;
}

// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
// { dg-output "contract violation in function member::T1::vfun at .*.C:37:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::vfun at .*.C:38:  .*(\n|\r\n|\r)" }
// { dg-output "vfun::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::fun at .*.C:45:  .*(\n|\r\n|\r)" }
// { dg-output "fun::x: 10(\n|\r\n|\r)" }
// { dg-output "main::f: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::fun2 at .*.C:52:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::fun2 at .*.C:53:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::fun2 at .*.C:54:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::fun2 at .*.C:55:  .*(\n|\r\n|\r)" }
// { dg-output "fun2::x: 10 fun2::y: 10.500000(\n|\r\n|\r)" }
// { dg-output "main::d: 10.500000(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::funend at .*.C:397:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function member::T1::funend at .*.C:398:  .*(\n|\r\n|\r)" }
// { dg-output "funend::x: 10(\n|\r\n|\r)" }
// { dg-output "main::s.z: 1(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::T1 at .*.C:85:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::T1 at .*.C:86:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::operator- at .*.C:99:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::operator- at .*.C:100:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::operator. at .*.C:92:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::operator. at .*.C:93:  .*(\n|\r\n|\r)" }
// { dg-output "==========(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::T2 at .*.C:113:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::T2 at .*.C:114:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::operator- at .*.C:127:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::operator- at .*.C:128:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::operator. at .*.C:120:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::operator. at .*.C:121:  .*(\n|\r\n|\r)" }
// { dg-output "==========(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::TC at .*.C:144:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::TC at .*.C:145:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::T1 at .*.C:85:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::T1 at .*.C:86:  .*(\n|\r\n|\r)" }
// { dg-output "==========(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::TC at .*.C:156:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::TC at .*.C:144:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::TC at .*.C:145:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::T1 at .*.C:85:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::T1 at .*.C:86:  .*(\n|\r\n|\r)" }
// { dg-output "==========(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::~TC at .*.C:151:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::~T1 at .*.C:106:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::TC::~TC at .*.C:151:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::~T1 at .*.C:106:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T2::~T2 at .*.C:134:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function special::T1::~T1 at .*.C:106:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:196:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:197:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:198:  .*(\n|\r\n|\r)" }
// { dg-output "T1::fun::m: -10, T1::fun::n: -20, T1::v: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:196:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:197:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:198:  .*(\n|\r\n|\r)" }
// { dg-output "T1::fun::m: -10, T1::fun::n: -20, T1::v: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3::fun at .*.C:211:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3::fun at .*.C:212:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3::fun at .*.C:213:  .*(\n|\r\n|\r)" }
// { dg-output "T3::fun::m: -10, T3::fun::n: -20, T3::v: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::fun at .*.C:247:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::fun at .*.C:248:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::fun at .*.C:249:  .*(\n|\r\n|\r)" }
// { dg-output "T3b::fun::m: -10, T3b::fun::n: -20, T3b::v: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::fun at .*.C:288:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::fun at .*.C:289:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::fun at .*.C:290:  .*(\n|\r\n|\r)" }
// { dg-output "T3c::fun::m: -10, T3c::fun::n: -20, T3c::v: -10(\n|\r\n|\r)" }
// { dg-output "=================(\n|\r\n|\r)" }
// { dg-output "T1:(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:196:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:197:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:198:  .*(\n|\r\n|\r)" }
// { dg-output "T1::fun::m: -1, T1::fun::n: -2, T1::v: -10(\n|\r\n|\r)" }
// { dg-output "=================(\n|\r\n|\r)" }
// { dg-output "T2:(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:196:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:197:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T1::fun at .*.C:198:  .*(\n|\r\n|\r)" }
// { dg-output "T1::fun::m: -1, T1::fun::n: -2, T1::v: -10(\n|\r\n|\r)" }
// { dg-output "=================(\n|\r\n|\r)" }
// { dg-output "T3:(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3::fun at .*.C:211:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3::fun at .*.C:212:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3::fun at .*.C:213:  .*(\n|\r\n|\r)" }
// { dg-output "T3::fun::m: -1, T3::fun::n: -2, T3::v: -10(\n|\r\n|\r)" }
// { dg-output "=================(\n|\r\n|\r)" }
// { dg-output "T3b:(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::fun at .*.C:247:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::fun at .*.C:248:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::fun at .*.C:249:  .*(\n|\r\n|\r)" }
// { dg-output "T3b::fun::m: -1, T3b::fun::n: -2, T3b::v: -10(\n|\r\n|\r)" }
// { dg-output "=================(\n|\r\n|\r)" }
// { dg-output "T3c:(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::fun at .*.C:288:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::fun at .*.C:289:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::fun at .*.C:290:  .*(\n|\r\n|\r)" }
// { dg-output "T3c::fun::m: -1, T3c::fun::n: -2, T3c::v: -10(\n|\r\n|\r)" }
// { dg-output "=============(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::p at .*.C:256:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::p at .*.C:257:  .*(\n|\r\n|\r)" }
// { dg-output "T3b::p: a: -3, v: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::u at .*.C:264:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::u at .*.C:265:  .*(\n|\r\n|\r)" }
// { dg-output "T3b::u: a: -3, z: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::n at .*.C:272:  .*(\n|\r\n|\r)" }
// { dg-output "T3b::n: a: -3(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3b::Sn at .*.C:279:  .*(\n|\r\n|\r)" }
// { dg-output "T3b::Sn: a: -3(\n|\r\n|\r)" }
// { dg-output "=============(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::p at .*.C:297:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::p at .*.C:298:  .*(\n|\r\n|\r)" }
// { dg-output "T3c::p: a: -3, v: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::u at .*.C:305:  .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::u at .*.C:306:  .*(\n|\r\n|\r)" }
// { dg-output "T3c::u: a: -3, z: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::n at .*.C:313:  .*(\n|\r\n|\r)" }
// { dg-output "T3c::n: a: -3(\n|\r\n|\r)" }
// { dg-output "contract violation in function virt::T3c::Sn at .*.C:320:  .*(\n|\r\n|\r)" }
// { dg-output "T3c::Sn: a: -3(\n|\r\n|\r)" }

