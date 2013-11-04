// { dg-do compile }
// { dg-options "-std=gnu++11" }

template<typename T, typename U> 
struct is_same 
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

#define CHECK_DECLTYPE(DECLTYPE,RESULT) \
  static_assert(is_same< DECLTYPE , RESULT >::value, #RESULT)

template<typename F> F create_a();

template<typename F, typename T1>
decltype(create_a<F&>()(create_a<const T1&>())) forward(F f, const T1& a1)
{
  return f(a1);
}

struct identity {
  template<typename T>
  const T& operator()(const T& x) { return x; }
};


identity id;
int i;
float f;

CHECK_DECLTYPE(decltype(forward(id, i)), const int&);
CHECK_DECLTYPE(decltype(forward(id, f)), const float&);
