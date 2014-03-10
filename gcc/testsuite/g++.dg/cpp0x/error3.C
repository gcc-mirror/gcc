// PR c++/47336
// { dg-do compile { target c++11 } }

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
