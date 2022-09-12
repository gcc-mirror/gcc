// { dg-do compile }

template<typename T>
void foo()
{
  T arr[20];
  // Reject array section in statement expression.
#pragma omp target map( ({ int x = 5; arr[0:x]; }) )
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {expected ';' before ':' token} "" { target *-*-* } .-2 }
// { dg-message {sorry, unimplemented: unsupported map expression '\(\{\.\.\.\}\)'} "" { target *-*-* } .-3 }
  { }
}

int main()
{
  int arr[20];
  // Reject array section in statement expression.
#pragma omp target map( ({ int x = 5; arr[0:x]; }) )
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {expected ';' before ':' token} "" { target *-*-* } .-2 }
// { dg-message {sorry, unimplemented: unsupported map expression '\(\{\.\.\.\}\)'} "" { target *-*-* } .-3 }
  { }

  foo<int> ();

  return 0;
}
