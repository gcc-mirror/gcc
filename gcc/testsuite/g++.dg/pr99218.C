// { dg-do compile }
// { dg-require-effective-target c++17 }

struct Data
{
  Data() {}
  ~Data() {}

  long long i;
};

struct X
{
  Data a;
  int b;
};

template<class T>
X get(T const&)
{
  return X{};
}

template<class... Ts>
struct pack_type : Ts...
{};

int main()
{
  pack_type<X>{get(1)};
}
