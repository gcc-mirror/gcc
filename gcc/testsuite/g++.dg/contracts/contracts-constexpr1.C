// ensure that passing pre/post do not affect constexpr functions
// ensure that failing pre/post generate an error at runtime in constexpr funcs
// { dg-do run }
// { dg-options "-std=c++20 -fcontracts -fcontract-continuation-mode=on" }

constexpr int wfun(int a)
  [[ pre: a > 0 ]]
  [[ post r: r > 0 ]]
{
  return a;
}

constexpr int ffun(int a)
  [[ pre: a > 0 ]]
  [[ post r: r > 0 ]]
{
  return a;
}

template<typename T>
constexpr int tfun(T a)
  [[ pre: a > 0 ]]
  [[ post r: r > 0 ]]
{
  return a;
}

template<typename T>
constexpr int wtfun(T a)
  [[ pre: a > 0 ]]
  [[ post r: r > 0 ]]
{
  return a;
}

template<typename T>
constexpr int ftfun(T a)
  [[ pre: a > 0 ]]
  [[ post r: r > 0 ]]
{
  return a;
}

constexpr int explicitfn(int a)
  [[ pre ignore: a > 0 ]]
  [[ pre check_maybe_continue: a > 0 ]]
  [[ post ignore r: r > 0 ]]
  [[ post check_maybe_continue r: r > 0 ]]
{
  return a;
}

int main(int, char **) {
  constexpr int a = wfun(10);
  int b = ffun(-10);
  constexpr int c = wtfun(10);
  int d = ftfun(-10);

  int e = explicitfn(-10);

  int z = ftfun(-10.0);

  return 0;
}

// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }
// { dg-output "contract violation in function ffun at .*.C:14: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ffun at .*.C:15: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ftfun<int> at .*.C:38: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ftfun<int> at .*.C:39: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function explicitfn at .*.C:46: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function explicitfn at .*.C:48: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ftfun<double> at .*.C:38: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ftfun<double> at .*.C:39: .*(\n|\r\n|\r)" }

