// { dg-do run { target c++11 } }
// PR c++/83160 failed to capture as lvalue

int main ()
{
  const int a = 0;

  if (![&a] (const int *p)
      {
	const int &b = a;
	// We should bind to the outer a
	return &b == p;
      } (&a))
    return 1;

  if (![&] (const int *p)
      {
	const int &b = a;
	// We should bind to the outer a
	return &b == p;
      } (&a))
    return 2;

  if ([=] (const int *p)
      {
	const int &b = a;
	// We should bind to the captured instance
	return &b == p;
      }(&a))
    return 3;

  return 0;
}
