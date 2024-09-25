// test that assumed contracts that reference undefined entities do not cause
// a link failure
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

int f(int t);

int dummy()
{
  [[ assert assume: f(1) > 0 ]];
  return -1;
}

int main()
{
  dummy();
  return 0;
}

