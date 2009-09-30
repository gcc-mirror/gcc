// Testcase for an extension to allow return type deduction when the lambda
// contains more than just a single return-statement.

// { dg-options -std=c++0x }
// { dg-do run }

bool b;
template <class T>
T f (T t)
{
  return [=] {
    auto i = t+1;
    if (b)
      return i+1;
    else
      return i+1;
  }();
}

int main()
{
  // Pointless, but well-formed.
  [] { return 1; return 2; }();

  if (f(1) != 3)
    return 1;
}
