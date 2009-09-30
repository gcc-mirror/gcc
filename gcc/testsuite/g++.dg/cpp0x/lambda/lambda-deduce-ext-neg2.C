// Test that in pedantic mode, we warn about the extension to allow return
// type deduction when the lambda contains more than just a single
// return-statement.

// { dg-options "-std=c++0x -pedantic" }

bool b;
template <class T>
T f (T t)
{
  [=] { return t+1; };		// OK
  return [=] {
    auto i = t+1;
    return i+1;			// { dg-warning "only statement" }
  }();
}

int main()
{
  if (f(1) != 3)
    return 1;
}
