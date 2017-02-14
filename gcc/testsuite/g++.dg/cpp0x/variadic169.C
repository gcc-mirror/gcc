// DR 2233
// { dg-do compile { target c++11 } }

template<typename ...T> void f(int n = 0, T ...t);

int main()
{
  f<int>();			// { dg-error "too few arguments" }
}
