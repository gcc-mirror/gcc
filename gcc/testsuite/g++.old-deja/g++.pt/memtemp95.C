// Build don't link:
// Origin: Mark Mitchell <mitchell@codesourcery.com>

template <class T, class V>
struct S
{
};

template <class T>
struct S<T, int>
{
  template <class U>
  void f (U);
};

template <class T>
template <class U>
void S<T, int>::f (U)
{
}
