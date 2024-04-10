// { dg-do compile }

template<typename T>
struct St {
  T *ptr;
};

template<typename T>
void foo()
{
  T arr[20];

  // Reject array section in compound initialiser.
#pragma omp target map( (struct St<T>) { .ptr = (T *) arr[5:5] } )
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {expected primary-expression before 'struct'} "" { target *-*-* } .-2 }
// { dg-error {expected '\)' before 'struct'} "" { target *-*-* } .-3 }
  { }

  // ...and this is unsupported too (probably not useful anyway).
#pragma omp target map( (struct St<T>) { .ptr = &arr[5] } )
// { dg-message {sorry, unimplemented: unsupported map expression 'St<int>\{\(\& arr\[5\]\)\}'} "" { target *-*-* } .-1 }
  { }
}

struct S {
  int *ptr;
};

int main()
{
  int arr[20];

  // Reject array section in compound initialiser.
#pragma omp target map( (struct S) { .ptr = (int *) arr[5:5] } )
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-warning {cast to pointer from integer of different size} "" { target lp64 } .-2 }
// { dg-error {expected primary-expression before 'struct'} "" { target *-*-* } .-3 }
// { dg-error {expected '\)' before 'struct'} "" { target *-*-* } .-4 }
  { }

  // ...and this is unsupported too (probably not useful anyway).
#pragma omp target map( (struct S) { .ptr = &arr[5] } )
// { dg-message {sorry, unimplemented: unsupported map expression 'S\{\(\& arr\[5\]\)\}'} "" { target *-*-* } .-1 }
  { }

  foo<int> ();

  return 0;
}
