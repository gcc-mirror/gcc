// PR c++/66260
// { dg-do compile { target c++14 } }

template <class>
bool foo = false;
template <>
bool foo<int> = true;
template <class T, int N>
bool foo<T[N]> = foo<T>;

#define assert(X) if (!(X)) __builtin_abort();

int main()
{
  // { dg-final { scan-assembler "_Z3fooIiE" } }
  assert(foo<int>);
  // { dg-final { scan-assembler "_Z3fooIdE" } }
  assert(!foo<double>);
  // { dg-final { scan-assembler "_Z3fooIA3_iE" } }
  assert(foo<int[3]>);
  // { dg-final { scan-assembler "_Z3fooIA3_dE" } }
  assert(!foo<double[3]>);
  // { dg-final { scan-assembler "_Z3fooIA2_A5_A3_iE" } }
  assert(foo<int[2][5][3]>);
  // { dg-final { scan-assembler "_Z3fooIA2_A5_A3_dE" } }
  assert(!foo<double[2][5][3]>);
}
