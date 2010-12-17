template <int I>
struct A { };

template <int J>
void f(A<1/J>);

template <int J>
void f(...) { }

int main()
{
  f<0>();
}

