// basic test to ensure pre contracts work for free functions
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

namespace nullary
{
  int x = 10;
  int y = 10;

  void fun()
    [[ pre: x < 0 ]]
  {
    printf("fun::x: %d\n", x);
  }

  void fun2()
    [[ pre: x < 0 ]]
    [[ pre: y < 0 ]]
  {
    printf("fun2::x: %d fun2::y: %d\n", x, y);
  }

  void funend()
    [[ pre: x < 0 ]];
}

namespace nonvoid
{
  int x = 10;
  double y = 10.5;

  struct S
  {
    bool z;
  };

  void vfun()
    [[ pre: x < 0 ]]
  {
    printf("vfun::x: %d\n", x);
  }

  int fun()
    [[ pre: x < 0 ]]
  {
    printf("fun::x: %d\n", x);
    return x;
  }

  double fun2()
    [[ pre: x < 0 ]]
    [[ pre: y < 0 ]]
  {
    printf("fun2::x: %d fun2::y: %f\n", x, y);
    return y;
  }

  S funend()
    [[ pre: x < 0 ]];
}

namespace nonnullary
{
  int x = 10;
  double y = 10.5;

  struct S
  {
    bool z;
  };

  void vfun(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]]
  {
    printf("vfun::x: %d\n", x);
  }

  int fun(int m, double n)
    [[ pre: x < 0 ]]
  {
    printf("fun::x: %d\n", x);
    return x;
  }

  double fun2(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: y < 0 ]]
    [[ pre: m < 0 ]]
    [[ pre: n < 0 ]]
  {
    printf("fun2::x: %d fun2::y: %f\n", x, y);
    return y;
  }

  S funend(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]];
}

int main(int, char**) {
  // nullary void
  {
    nullary::fun();
    nullary::fun2();
    nullary::funend();
  }

  // nullary non void
  {
    nonvoid::vfun();

    int f = 13;
    f = nonvoid::fun();
    printf("main::f: %d\n", f);
    double d = 13.37;
    d = nonvoid::fun2();
    printf("main::d: %f\n", d);
    nonvoid::S s = nonvoid::funend();
    printf("main::s.z: %d\n", s.z ? 1 : 0);
  }

  // non-nullary non-void
  {
    int x = 11;
    double y = 11.5;

    nonnullary::vfun(x, y);

    int f = 13;
    f = nonnullary::fun(x, y);
    printf("main::f: %d\n", f);
    double d = 13.37;
    d = nonnullary::fun2(x, y);
    printf("main::d: %f\n", d);
    nonnullary::S s = nonnullary::funend(x, y);
    printf("main::s.z: %d\n", s.z ? 1 : 0);
  }
  return 0;
}

namespace nullary
{
  void funend()
    [[ pre: x < 0 ]]
  {
    printf("funend::x: %d\n", x);
  }
}

namespace nonvoid
{
  S funend()
    [[ pre: x < 0 ]]
  {
    printf("funend::x: %d\n", x);
    S s;
    s.z = true;
    return s;
  }
}

namespace nonnullary
{
  S funend(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]]
  {
    printf("funend::x: %d\n", x);
    S s;
    s.z = true;
    return s;
  }
}

// { dg-output "contract violation in function nullary::fun at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "fun::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nullary::fun2 at .*.C:18: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nullary::fun2 at .*.C:19: .*(\n|\r\n|\r)" }
// { dg-output "fun2::x: 10 fun2::y: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nullary::funend at .*.C:146: .*(\n|\r\n|\r)" }
// { dg-output "funend::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonvoid::vfun at .*.C:39: .*(\n|\r\n|\r)" }
// { dg-output "vfun::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonvoid::fun at .*.C:45: .*(\n|\r\n|\r)" }
// { dg-output "fun::x: 10(\n|\r\n|\r)" }
// { dg-output "main::f: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonvoid::fun2 at .*.C:52: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonvoid::fun2 at .*.C:53: .*(\n|\r\n|\r)" }
// { dg-output "fun2::x: 10 fun2::y: 10.500000(\n|\r\n|\r)" }
// { dg-output "main::d: 10.500000(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonvoid::funend at .*.C:155: .*(\n|\r\n|\r)" }
// { dg-output "funend::x: 10(\n|\r\n|\r)" }
// { dg-output "main::s.z: 1(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::vfun at .*.C:74: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::vfun at .*.C:75: .*(\n|\r\n|\r)" }
// { dg-output "vfun::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::fun at .*.C:81: .*(\n|\r\n|\r)" }
// { dg-output "fun::x: 10(\n|\r\n|\r)" }
// { dg-output "main::f: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::fun2 at .*.C:88: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::fun2 at .*.C:89: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::fun2 at .*.C:90: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::fun2 at .*.C:91: .*(\n|\r\n|\r)" }
// { dg-output "fun2::x: 10 fun2::y: 10.500000(\n|\r\n|\r)" }
// { dg-output "main::d: 10.500000(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::funend at .*.C:167: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nonnullary::funend at .*.C:168: .*(\n|\r\n|\r)" }
// { dg-output "funend::x: 10(\n|\r\n|\r)" }
// { dg-output "main::s.z: 1(\n|\r\n|\r)" }

