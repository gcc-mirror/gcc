// { dg-do compile { target c++14 } }

bool b;
auto f()
{
  if (b)
    return 42;
  else
    return f();
}
