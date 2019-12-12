// { dg-do compile { target c++11 } }

void f(int i)
{
  if (i) [[likely]]
    {
      ++i;
    }
}
