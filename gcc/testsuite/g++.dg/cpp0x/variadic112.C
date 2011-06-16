// PR c++/49420
// { dg-options -std=c++0x }

struct A { };

template <class T> struct B
{
  typedef typename T::type type ; // { dg-error "no type" }
};

template <typename Array, typename... Args>
typename B<Array>::type
get(const Array& a, Args... args);

int main()
{
  A a;
  int x = get(a, 1, 2, 3);	// { dg-error "no match" }
}
