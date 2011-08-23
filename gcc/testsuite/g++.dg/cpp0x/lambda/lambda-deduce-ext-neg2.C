// Test that this is accepted even when pedantic now that it's part
// of the standard.

// { dg-options "-std=c++0x -pedantic" }

bool b;
template <class T>
T f (T t)
{
  [=] { return t+1; };		// OK
  return [=] {
    auto i = t+1;
    return i+1;
  }();
}

int main()
{
  if (f(1) != 3)
    return 1;
}
