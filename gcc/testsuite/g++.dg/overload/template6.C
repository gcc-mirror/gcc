// { dg-do compile { target c++11 } }

template <typename>
struct is_function {
  static constexpr bool value = false;
};

template <typename R, typename ...Args>
struct is_function<R(Args...)>
{
  static constexpr bool value = true;
};

template<bool, typename> struct enable_if {};

template<typename T> struct enable_if<true, T> 
{
  typedef T type;
};

template <class T>
struct remove_pointer
{
  typedef T type;
};

template <class T>
struct remove_pointer<T*>
{
  typedef T type;
};

void f(int) {}
void f(double) {}

template <class T>
struct X
{
  template <class U=T,
	    typename enable_if<is_function<
				 typename remove_pointer<U>::type>::value,
			       bool>::type = false> X(U&&) {}
};

int main() {
  X<void(*)(int)> x0(f);
}
