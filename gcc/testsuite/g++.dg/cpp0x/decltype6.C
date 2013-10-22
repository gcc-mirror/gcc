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

template<typename T> const T& foo(); 


int i; 

template<typename T>
struct A 
{ 
  double x; 
};

const A<double>* a = new A<double>(); 

static_assert(is_same<decltype(foo<int>()), const int&>::value,
              "type should be const int&");
static_assert(is_same<decltype(i), int>::value,
              "type should be int");
static_assert(is_same<decltype(a->x), double>::value,
              "type should be double");
static_assert(is_same<decltype((a->x)), const double&>::value,
              "type should be const double&");
