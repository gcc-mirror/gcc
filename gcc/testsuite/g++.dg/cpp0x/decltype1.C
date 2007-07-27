// { dg-do "compile" }
// { dg-options "-std=gnu++0x" }

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

const int& foo(); 
int i; 
struct A { double x; };
const A* a = new A(); 

static_assert(is_same<decltype(foo()), const int&>::value,
              "type should be const int&");
static_assert(is_same<decltype(i), int>::value,
              "type should be int");
static_assert(is_same<decltype(a->x), double>::value,
              "type should be double");
static_assert(is_same<decltype((a->x)), const double&>::value,
              "type should be const double&");
