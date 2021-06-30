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

// { dg-output "default std::handle_contract_violation called: .*.C 37 member::T1::vfun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 38 member::T1::vfun .*(\n|\r\n|\r)*" }
// { dg-output "vfun::x: 10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 45 member::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "fun::x: 10(\n|\r\n|\r)*" }
// { dg-output "main::f: 10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 52 member::T1::fun2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 53 member::T1::fun2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 54 member::T1::fun2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 member::T1::fun2 .*(\n|\r\n|\r)*" }
// { dg-output "fun2::x: 10 fun2::y: 10.500000(\n|\r\n|\r)*" }
// { dg-output "main::d: 10.500000(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 397 member::T1::funend .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 398 member::T1::funend .*(\n|\r\n|\r)*" }
// { dg-output "funend::x: 10(\n|\r\n|\r)*" }
// { dg-output "main::s.z: 1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 85 special::T1::T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 86 special::T1::T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 99 special::T1::operator- .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 100 special::T1::operator- .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 92 special::T1::operator. .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 93 special::T1::operator. .*(\n|\r\n|\r)*" }
// { dg-output "==========(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 113 special::T2::T2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 114 special::T2::T2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 127 special::T2::operator- .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 128 special::T2::operator- .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 120 special::T2::operator. .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 121 special::T2::operator. .*(\n|\r\n|\r)*" }
// { dg-output "==========(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 144 special::TC::TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 145 special::TC::TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 85 special::T1::T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 86 special::T1::T1 .*(\n|\r\n|\r)*" }
// { dg-output "==========(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 156 special::TC::TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 144 special::TC::TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 145 special::TC::TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 85 special::T1::T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 86 special::T1::T1 .*(\n|\r\n|\r)*" }
// { dg-output "==========(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 151 special::TC::~TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 106 special::T1::~T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 151 special::TC::~TC .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 106 special::T1::~T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 134 special::T2::~T2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 106 special::T1::~T1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 196 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 197 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 198 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "T1::fun::m: -10, T1::fun::n: -20, T1::v: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 196 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 197 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 198 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "T1::fun::m: -10, T1::fun::n: -20, T1::v: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 211 virt::T3::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 212 virt::T3::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 213 virt::T3::fun .*(\n|\r\n|\r)*" }
// { dg-output "T3::fun::m: -10, T3::fun::n: -20, T3::v: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 247 virt::T3b::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 248 virt::T3b::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 249 virt::T3b::fun .*(\n|\r\n|\r)*" }
// { dg-output "T3b::fun::m: -10, T3b::fun::n: -20, T3b::v: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 288 virt::T3c::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 289 virt::T3c::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 290 virt::T3c::fun .*(\n|\r\n|\r)*" }
// { dg-output "T3c::fun::m: -10, T3c::fun::n: -20, T3c::v: -10(\n|\r\n|\r)*" }
// { dg-output "=================(\n|\r\n|\r)*" }
// { dg-output "T1:(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 196 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 197 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 198 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "T1::fun::m: -1, T1::fun::n: -2, T1::v: -10(\n|\r\n|\r)*" }
// { dg-output "=================(\n|\r\n|\r)*" }
// { dg-output "T2:(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 196 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 197 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 198 virt::T1::fun .*(\n|\r\n|\r)*" }
// { dg-output "T1::fun::m: -1, T1::fun::n: -2, T1::v: -10(\n|\r\n|\r)*" }
// { dg-output "=================(\n|\r\n|\r)*" }
// { dg-output "T3:(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 211 virt::T3::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 212 virt::T3::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 213 virt::T3::fun .*(\n|\r\n|\r)*" }
// { dg-output "T3::fun::m: -1, T3::fun::n: -2, T3::v: -10(\n|\r\n|\r)*" }
// { dg-output "=================(\n|\r\n|\r)*" }
// { dg-output "T3b:(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 247 virt::T3b::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 248 virt::T3b::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 249 virt::T3b::fun .*(\n|\r\n|\r)*" }
// { dg-output "T3b::fun::m: -1, T3b::fun::n: -2, T3b::v: -10(\n|\r\n|\r)*" }
// { dg-output "=================(\n|\r\n|\r)*" }
// { dg-output "T3c:(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 288 virt::T3c::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 289 virt::T3c::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 290 virt::T3c::fun .*(\n|\r\n|\r)*" }
// { dg-output "T3c::fun::m: -1, T3c::fun::n: -2, T3c::v: -10(\n|\r\n|\r)*" }
// { dg-output "=============(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 256 virt::T3b::p .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 257 virt::T3b::p .*(\n|\r\n|\r)*" }
// { dg-output "T3b::p: a: -3, v: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 264 virt::T3b::u .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 265 virt::T3b::u .*(\n|\r\n|\r)*" }
// { dg-output "T3b::u: a: -3, z: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 272 virt::T3b::n .*(\n|\r\n|\r)*" }
// { dg-output "T3b::n: a: -3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 279 virt::T3b::Sn .*(\n|\r\n|\r)*" }
// { dg-output "T3b::Sn: a: -3(\n|\r\n|\r)*" }
// { dg-output "=============(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 297 virt::T3c::p .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 298 virt::T3c::p .*(\n|\r\n|\r)*" }
// { dg-output "T3c::p: a: -3, v: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 305 virt::T3c::u .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 306 virt::T3c::u .*(\n|\r\n|\r)*" }
// { dg-output "T3c::u: a: -3, z: -10(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 313 virt::T3c::n .*(\n|\r\n|\r)*" }
// { dg-output "T3c::n: a: -3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 320 virt::T3c::Sn .*(\n|\r\n|\r)*" }
// { dg-output "T3c::Sn: a: -3(\n|\r\n|\r)*" }

