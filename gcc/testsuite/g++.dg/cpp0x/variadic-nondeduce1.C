// { dg-do compile { target c++11 } }

template <class... T>
void f(T..., int, T...) { }

int main()
{
  f(0);
  f<int>(0,0,0);
  f<int,int>(0,0,0,0,0);
  f(0,0,0);			// { dg-error "" }
}
