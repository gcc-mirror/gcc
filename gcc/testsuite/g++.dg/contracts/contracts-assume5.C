// test that assumed constexpr contracts that reference defined entities, or
// undefined entities in unevaluated context, cause constexpr eval failure when
// the predicate is constexpr false
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

constexpr int f(int t)
{
  return -1;
}

constexpr int dummy()
{
  [[ assert assume: f(1) > 0 ]];
  return -1;
}

constexpr int undef(int t);

constexpr int dummy2()
{
  [[ assert assume: sizeof(decltype(undef(1))) < 0 ]];
  return -1;
}

int main()
{
  constexpr int n = dummy(); // { dg-message "in .constexpr. expansion" }
  // { dg-error "contract predicate" "" { target *-*-* } 14 }
  constexpr int m = dummy2(); // { dg-message "in .constexpr. expansion" }
  // { dg-error "contract predicate" "" { target *-*-* } 22 }
  return 0;
}

