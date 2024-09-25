// check that contracts work around deduced return types
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

auto g0(int a) [[ pre: a < 0 ]] [[ post r: r > 0 ]]
{
  return -a * 1.2;
}

int g1(int m) [[ post r: r == m ]];

int g1(int n) [[ post s: s == n ]]
{
  return -n;
}

int g2(int z)
{
  return -z;
}

int g3(int z)
  [[ pre: sizeof(decltype(g2(5))) > 4 ]]
{
  return -z;
}

auto g4(int m) [[ post: m ]];

auto g4(int m) [[ post: m ]]
{
  return -m;
}

auto g5(int m) [[ pre: m ]];

auto g5(int m) [[ pre: m ]]
{
  return -m;
}

template<typename T>
auto g6(T t) [[ post r: r == t ]];

template<typename S>
auto g6(S s) [[ post q: q == s ]]
{
  return -s;
}

// template<typename T>
// T g7(T t) [[ post r: r == t ]];

template<typename S>
S g7(S s) [[ post q: q == s ]]
{
  return -s;
}

int main(int, char**) {
  g0(5);
  g1(6);
  g2(1);
  g3(1);
  g4(0);
  g5(0);
  g6(5);
  g6(5.5);
  g7(5);
  g7(6.6);
  return 0;
}

// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }
// { dg-output "contract violation in function g0 at .*.C:5: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g0 at .*.C:5: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g1 at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g3 at .*.C:23: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g4 at .*.C:30: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g5 at .*.C:37: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g6<int> at .*.C:46: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g6<double> at .*.C:46: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g7<int> at .*.C:55: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function g7<double> at .*.C:55: .*(\n|\r\n|\r)" }

