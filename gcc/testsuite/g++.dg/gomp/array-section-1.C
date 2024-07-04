// { dg-do compile }
// { dg-additional-options "-fdump-tree-original" }

int x;

template<int C, int D>
void foo()
{
  int arr1[40];
#pragma omp target map(arr1[x ? C : D])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] \[len: [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[x ? C : D : D])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] \[len: [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[1 : x ? C : D])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[1\] \[len: x != 0 \? [0-9]+ : [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: [0-9]+\]\)} "original" } }
  { }
}

int main()
{
  int arr1[40];
#pragma omp target map(arr1[x ? 3 : 5])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] \[len: [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[x ? 3 : 5 : 5])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] \[len: [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x != 0 \? 3 : 5>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[1 : x ? 3 : 5])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[1\] [len: x != 0 ? [0-9]+ : [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: [0-9]+\]\)} "original" } }
  { }

  foo<3, 5> ();

  return 0;
}

