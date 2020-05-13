// { dg-do compile { target c++20 } }

template <int N>
struct A { };

template <int N>
void g(A<[]{return N;}()>) {}

int main()
{
  g<1>({});
}
