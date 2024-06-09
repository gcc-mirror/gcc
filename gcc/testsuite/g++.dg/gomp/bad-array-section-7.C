// { dg-do compile }

int x;

template<typename T>
void foo()
{
  T arr[20];
  // Here we know the type of the array section (the upper bound is reported)...
#pragma omp target map(arr[5:5] * 2)
// { dg-error {invalid operands of types 'int \[10\]' and 'int'} "" { target *-*-* } .-1 }
  { }
  /* ...but here, we have an incomplete array type because of the variable
     low bound 'x'.  */
#pragma omp target map(arr[x:5] * 2)
// { dg-error {invalid operands of types 'int \[\]' and 'int'} "" { target *-*-* } .-1 }
  { }
}

int main()
{
  int arr[20];
  // Here we know the type of the array section (the upper bound is reported)...
#pragma omp target map(arr[5:5] * 2)
// { dg-error {invalid operands of types 'int \[10\]' and 'int'} "" { target *-*-* } .-1 }
  { }
  /* ...but here, we have an incomplete array type because of the variable
     low bound 'x'.  */
#pragma omp target map(arr[x:5] * 2)
// { dg-error {invalid operands of types 'int \[\]' and 'int'} "" { target *-*-* } .-1 }
  { }

  foo<int> ();

  return 0;
}
