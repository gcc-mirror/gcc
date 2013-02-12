// Test for int128 arithmetic conversions.
// { dg-do compile { target int128 } }
// { dg-options "-std=gnu++11" }

template <class T, class U> struct same;
template <class T> struct same<T,T> { };
#define assert_same(T,U) (same<T,U>())

int main()
{
  long long ll;
  unsigned long long ull;
  __int128 i8;
  unsigned __int128 u8;
  assert_same (decltype (ll+i8), __int128);
  assert_same (decltype (ull+i8), __int128);
  assert_same (decltype (ll+u8), unsigned __int128);
  assert_same (decltype (ull+u8), unsigned __int128);
}
