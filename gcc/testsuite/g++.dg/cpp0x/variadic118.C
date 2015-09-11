// { dg-do compile { target c++11 } }

template <class... T>
void f(T... ts);

struct B { };
int main()
{
  f<int>(B(), 1);		// { dg-error "" }
}
