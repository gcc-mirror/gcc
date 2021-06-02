// { dg-do compile { target c++11 } }
// { dg-options "-Wno-psabi -w" }

template <typename T1, typename T2, int ...args>
struct shufflevector
{
  static auto shuffle (T1 a, T2 b)
      -> decltype (__builtin_shufflevector (a, b, args...))
  {
    return __builtin_shufflevector (a, b, args...);
  }
};

typedef int v4si __attribute__((vector_size (16)));
v4si a, b, c;
int main()
{
  c = shufflevector<v4si, v4si, 0, 1, 4, 5>::shuffle (a, b);
}
