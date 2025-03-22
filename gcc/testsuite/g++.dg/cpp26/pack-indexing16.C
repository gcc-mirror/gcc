// { dg-do compile { target c++26 } }

int i;
constexpr int idx()
{
  if consteval { return 0; }
  else { return i; }
}

template <int... Ns>
int first () { return Ns...[idx()]; }

int main()
{
  return first<0,1,2>();
}
