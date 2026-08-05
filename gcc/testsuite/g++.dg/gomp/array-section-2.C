// { dg-do compile }
// { dg-additional-options "-fdump-tree-original" }

int x, y;

class C {
  int x, y;

public:
  int foo();
};

int C::foo()
{
  int arr1[40];
  /* There is a parsing ambiguity here without the space.  We don't try to
     resolve that automatically (though maybe we could, in theory).  */
#pragma omp target map(arr1[::x: ::y])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x>\] \[len: \(sizetype\) y \* [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[::x:])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x>\] \[len: \(40 - \(sizetype\) SAVE_EXPR <x>\) \* [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[: ::y])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[0\] \[len: \(sizetype\) y \* [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: 0\]\)} "original" } }
  { }
  return ::x + ::y;
}

template<typename T>
class Ct {
  T x, y;

public:
  void foo();
};

template<typename T>
void Ct<T>::foo()
{
  int arr1[40];
#pragma omp target map(arr1[::x: ::y])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x>\] \[len: \(sizetype\) y \* [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[::x:])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[SAVE_EXPR <x>\] \[len: \(40 - \(sizetype\) SAVE_EXPR <x>\) \* [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: \((?:long )?int\) &arr1\[SAVE_EXPR <x>\] - \((?:long )?int\) &arr1\]\)} "original" } }
  { }
#pragma omp target map(arr1[: ::y])
// { dg-final { scan-tree-dump {map\(tofrom:arr1\[0\] \[len: \(sizetype\) y \* [0-9]+\]\) map\(firstprivate:arr1 \[pointer assign, bias: 0\]\)} "original" } }
  { }
}

int main()
{
  C c;
  Ct<int> ct;

  c.foo ();
  ct.foo ();

  return 0;
}

