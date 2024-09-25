// basic test to ensure contracts generalized redecl works
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

namespace defining
{
  int x = 10;
  double y = 10.5;

  struct S
  {
    bool z;
  };

  struct T1
  {
    void vfun(int m, double n);
    int fun(int m, double n);
    double fun2(int m, double n);
    S funend(int m, double n);
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

namespace nondefining
{
  int x = 10;
  double y = 10.5;

  struct S
  {
    bool z;
  };

  struct T1
  {
    void vfun(int m, double n);
    int fun(int m, double n);
    double fun2(int m, double n);
    S funend(int m, double n);
  };

  void T1::vfun(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]];
  int T1::fun(int m, double n)
    [[ pre: x < 0 ]];
  double T1::fun2(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: y < 0 ]]
    [[ pre: m < 0 ]]
    [[ pre: n < 0 ]];

  void T1::vfun(int m, double n)
  {
    printf("vfun::x: %d\n", x);
  }

  int T1::fun(int m, double n)
  {
    printf("fun::x: %d\n", x);
    return x;
  }

  double T1::fun2(int m, double n)
  {
    printf("fun2::x: %d fun2::y: %f\n", x, y);
    return y;
  }
}

int main(int, char**) {
  // defining redecl
  {
    int x = 11;
    double y = 11.5;

    defining::T1 t1;
    t1.vfun(x, y);

    int f = 13;
    f = t1.fun(x, y);
    printf("main::f: %d\n", f);
    double d = 13.37;
    d = t1.fun2(x, y);
    printf("main::d: %f\n", d);
    defining::S s = t1.funend(x, y);
    printf("main::s.z: %d\n", s.z ? 1 : 0);
  }

  // nondefining redecl
  {
    int x = 12;
    double y = 12.5;

    nondefining::T1 t1;
    t1.vfun(x, y);

    int f = 13;
    f = t1.fun(x, y);
    printf("main::f: %d\n", f);
    double d = 13.37;
    d = t1.fun2(x, y);
    printf("main::d: %f\n", d);
    nondefining::S s = t1.funend(x, y);
    printf("main::s.z: %d\n", s.z ? 1 : 0);
  }
  return 0;
}

namespace defining
{
  S T1::funend(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]]
  {
    printf("funend::x: %d\n", x);
    S s;
    s.z = true;
    return s;
  }
}

namespace nondefining
{
  S T1::funend(int m, double n)
    [[ pre: x < 0 ]]
    [[ pre: m < 0 ]];

  S T1::funend(int m, double n)
  {
    printf("funend::x: %d\n", x);
    S s;
    s.z = true;
    return s;
  }
}

// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
// { dg-output "contract violation in function defining::T1::vfun at .*.C:25: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::vfun at .*.C:26: .*(\n|\r\n|\r)" }
// { dg-output "vfun::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::fun at .*.C:32: .*(\n|\r\n|\r)" }
// { dg-output "fun::x: 10(\n|\r\n|\r)" }
// { dg-output "main::f: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::fun2 at .*.C:39: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::fun2 at .*.C:40: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::fun2 at .*.C:41: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::fun2 at .*.C:42: .*(\n|\r\n|\r)" }
// { dg-output "fun2::x: 10 fun2::y: 10.500000(\n|\r\n|\r)" }
// { dg-output "main::d: 10.500000(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::funend at .*.C:138: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function defining::T1::funend at .*.C:139: .*(\n|\r\n|\r)" }
// { dg-output "funend::x: 10(\n|\r\n|\r)" }
// { dg-output "main::s.z: 1(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::vfun at .*.C:68: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::vfun at .*.C:69: .*(\n|\r\n|\r)" }
// { dg-output "vfun::x: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::fun at .*.C:71: .*(\n|\r\n|\r)" }
// { dg-output "fun::x: 10(\n|\r\n|\r)" }
// { dg-output "main::f: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::fun2 at .*.C:73: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::fun2 at .*.C:74: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::fun2 at .*.C:75: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::fun2 at .*.C:76: .*(\n|\r\n|\r)" }
// { dg-output "fun2::x: 10 fun2::y: 10.500000(\n|\r\n|\r)" }
// { dg-output "main::d: 10.500000(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::funend at .*.C:151: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function nondefining::T1::funend at .*.C:152: .*(\n|\r\n|\r)" }
// { dg-output "funend::x: 10(\n|\r\n|\r)" }
// { dg-output "main::s.z: 1(\n|\r\n|\r)" }

