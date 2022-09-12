// { dg-do compile }

int x;

template<typename X>
struct Tt {
  X arr[20];
};

template<typename X>
struct St {
  X *tvec;
};

template<typename T>
void foo()
{
  struct St<Tt<T> > *s;
  // You can't use an array section like this.  Make sure sensible errors are
  // reported.
#pragma omp target map(s->tvec[3:5].arr[0:20])
// { dg-error {request for member 'arr' in 's->St<Tt<int> >::tvec\[3:5\]', which is of non-class type 'Tt<int> \[8\]'} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(s->tvec[5:x].arr[0:20])
// { dg-error {invalid use of array with unspecified bounds} "" { target *-*-* } .-1 }
  { }
}

struct T {
  int arr[20];
};

struct S {
  struct T *tvec;
};

int main()
{
  struct S *s;
  // You can't use an array section like this.  Make sure sensible errors are
  // reported.
#pragma omp target map(s->tvec[3:5].arr[0:20])
// { dg-error {request for member 'arr' in 's->S::tvec\[3:5\]', which is of non-class type 'T \[8\]'} "" { target *-*-* } .-1 }
  { }
#pragma omp target map(s->tvec[5:x].arr[0:20])
// { dg-error {invalid use of array with unspecified bounds} "" { target *-*-* } .-1 }
// { dg-error {expected '\)' before 'arr'} "" { target *-*-* } .-2 }
  { }

  foo<int> ();

  return 0;
}
