// PR c++/24761
// { dg-do compile }

template <int>
int f (int i)
{
  asm ("# %0 %1" : "+r" (i));
  return i;
}

int main ()
{
  return f<0> (0) + f<1> (0);
}
