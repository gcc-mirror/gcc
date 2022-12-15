// Tests to ensure that deduced return types work with postconditions using
// the return value on defining declarations.
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts --param ggc-min-heapsize=0 --param ggc-min-expand=0" }

auto undeduced(int z)
{
  if (!(sizeof(decltype(undeduced(5))) > 4)) // { dg-error "before deduction" }
    return 5;
  return 4;
}

// defining declaration, fine
auto g0(int a) [[ pre: a < 0 ]] [[ post r: r > 0 ]]
{
  return -a * 1.2;
}

// non defining post using nondeduced identifier, fine
int g1(int m) [[ post r: r == m ]];

int g1(int n) [[ post s: s == n ]]
{
  return -n;
}

int g2(int z)
  [[ pre: sizeof(decltype(g2(5))) > 4 ]]; // { dg-error "not declared" }

int g3(int z)
  [[ pre: sizeof(decltype(g2(5))) > 4 ]]
{
  return -z;
}

// deduced that doesn't use return, good
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
  return s;
}

template<typename T>
T g7(T t) [[ post r: r == t ]];

template<typename S>
S g7(S s) [[ post q: q == s ]]
{
  return s;
}

template<typename T>
auto g8(T t) [[ post r: r == t && sizeof(decltype(::g8(t))) > 2 ]]; // { dg-error "not been declared" }

// This failure is related to the fact that we've invalidated the previous
// contract. 
template<typename S>
auto g8(S s) [[ post q: q == s && sizeof(decltype(::g8(s))) > 2 ]] // { dg-error "mismatched" }
{
  return s;
}

// non defining pre, bad
auto f0(int z)
  [[ pre: sizeof(decltype(f0(5))) > 4 ]]; // { dg-error "not declared" }

// defining pre, still ill formed
auto f1(int z)
  [[ pre: sizeof(decltype(f1(5))) > 4 ]] // { dg-error "not declared" }
{
  return '5';
}

// undeduced using postcon, OK
auto f2(int m) [[ post r: r == m ]];

auto f2(int n) [[ post s: s == n ]]
{
  return n;
}

template<typename T>
void f3(T t) [[ post r: false ]] // { dg-error "function does not return a value" }
{
}

int main(int, char**)
{
  return 0;
}

