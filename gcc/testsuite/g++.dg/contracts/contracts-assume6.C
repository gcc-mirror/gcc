// ensure that non-defined entities in assume contracts do not error
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

template<typename T>
T id2(T n);

int fun(int n)
  [[ pre assume: id2(n) > 0 ]]
  [[ pre: n > 0 ]]
{
  return -n;
}

template<typename T>
T tfun(T n)
  [[ pre assume: id2(n) > 0 ]]
  [[ pre: n > 0 ]]
{
  return -n;
}

template<typename T>
constexpr T id(T n); // { dg-warning "used but never defined" }

template<typename T>
constexpr T cfun(T n)
  [[ pre assume: id(n) > 0 ]]
  [[ pre: id(n) > 0 ]] // { dg-error "used before its definition" }
{
  return -n;
}

template<typename T>
constexpr T id3(T n)
{
  return n;
}

template<typename T>
constexpr T cfun2(T n)
  [[ pre assume: id3(n) > 0 ]] // { dg-error "contract predicate" }
{
  return -n;
}

template<typename T>
constexpr T cfun3(T n)
  [[ pre: id3(n) > 0 ]] // { dg-error "contract predicate" }
{
  return -n;
}

int main() {
  constexpr int n = cfun(-5);
  constexpr int n2 = cfun2(-5);
  constexpr int n3 = cfun3(-5);
  fun(-5);
  tfun(-5);
}

