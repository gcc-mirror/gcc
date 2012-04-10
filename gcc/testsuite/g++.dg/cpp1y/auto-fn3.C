// { dg-options -std=c++1y }

bool b;
auto f()
{
  if (b)
    return 42;
  else
    return f();
}
