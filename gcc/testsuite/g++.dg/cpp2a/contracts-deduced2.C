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

// { dg-output "default std::handle_contract_violation called: .*.C 5 g0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 5 g0 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 12 g1 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 23 g3 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 30 g4 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 37 g5 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 46 g6<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 46 g6<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 g7<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 55 g7<double> .*(\n|\r\n|\r)*" }

