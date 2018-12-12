// P0595R1
// { dg-do run { target c++14 } }

struct false_type { static constexpr bool value = false; };
struct true_type { static constexpr bool value = true; };
template<class T, class U>
struct is_same : false_type {};
template<class T>
struct is_same<T, T> : true_type {};

int a[__builtin_is_constant_evaluated () ? 1 : 2];
int b[1];
static_assert (is_same<decltype (a), decltype (b)>::value, "");

int
main ()
{
  int c[__builtin_is_constant_evaluated () ? 3 : 4];
  int d[3];
  static_assert (is_same<decltype (c), decltype (d)>::value, "");
  int (*e)[7][9] = new int[__builtin_is_constant_evaluated () ? -1 : 5]
			  [__builtin_is_constant_evaluated () ? 7 : 8]
			  [__builtin_is_constant_evaluated () ? 9 : 10];
  e[0][0][0] = 6;
  delete[] e;
}
