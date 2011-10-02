// { dg-options -std=c++0x }

template <class... T>
void f(T..., int, T...) { }

int main()
{
  f(0);
  f<int>(0,0,0);
  f<int,int>(0,0,0,0,0);
  f(0,0,0);			// { dg-error "" }
}
