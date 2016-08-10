// { dg-do compile { target c++11 } }
// { dg-options "-w" }

void f()
{
  if constexpr (false)
    {
    l:;
      goto l;
    }
  else
    {
    }
}
