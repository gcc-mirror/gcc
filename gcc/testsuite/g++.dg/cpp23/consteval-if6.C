// { dg-do compile { target c++20 } }
// { dg-options "-w" }

void f()
{
  if consteval
    {
      goto l;
    l:;
    }
  else
    {
      goto l2;
    l2:;
    }
}
