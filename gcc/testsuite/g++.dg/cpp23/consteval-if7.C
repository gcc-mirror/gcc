// { dg-do compile { target c++20 } }
// { dg-options "-w" }

void f()
{
  if not consteval
    {
    l:;
      goto l;
    }
  else
    {
    l2:;
      goto l2;
    }
}
