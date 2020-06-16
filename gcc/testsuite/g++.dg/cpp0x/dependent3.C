// { dg-do compile { target c++11 } }

template<typename c>
struct d
{
  using e = c;
};

template<class f>
struct g
{
  using h = typename d<f>::e;

  template<class i, class j>
  auto operator()(i, j k) -> decltype(h{k});
};

template<class l>
void m()
{
  int a[1];
  l{}(a, a);
}

int main()
{
  m<g<int *>>();
}
