// { dg-do compile }

bool partly = false;

template<typename T>
void foo()
{
  T arr[20];
#pragma omp target map(partly ? arr[5:5] : arr)
// { dg-message {sorry, unimplemented: unsupported map expression '\(partly \? \(\(int\*\)\(\& arr\[5:5\]\)\) : \(\(int\*\)\(\& arr\)\)\)'} "" { target *-*-* } .-1 }
  { }
}

int main()
{
  int arr[20];
#pragma omp target map(partly ? arr[5:5] : arr)
// { dg-message {sorry, unimplemented: unsupported map expression '\(partly \? \(\(int\*\)\(\& arr\[5:5\]\)\) : \(\(int\*\)\(\& arr\)\)\)'} "" { target *-*-* } .-1 }
  { }

  foo<int> ();

  return 0;
}
