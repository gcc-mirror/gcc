// PR c++/47336
// { dg-options -std=c++0x }

template <typename T>
void g(T t)
{
  t+1;				// { dg-error "no match" }
}

template <typename S>
class C
{
  struct D {} d;
public:
  decltype(g(d)) h()
  {
    return g(d);
  }
};

int main()
{
  C<int>().h();
}
