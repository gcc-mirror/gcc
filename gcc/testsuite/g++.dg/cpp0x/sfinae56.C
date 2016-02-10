// PR c++/68926
// { dg-do compile { target c++11 } }

struct true_type { static constexpr bool value = true; };
struct false_type { static constexpr bool value = false; };

template<bool Cond> struct enable_if { using type = void; };
template<> struct enable_if<false> { };

template<typename T, typename U> struct is_same : false_type { };
template<typename T> struct is_same<T, T> : true_type { };

template<typename T>
typename enable_if<is_same<int, T>::value>::type
func();

template<typename T, typename = decltype(func<T>)>
true_type test(T);

false_type test(...);

int main()
{
   decltype(test(0))::value;   // ok
   decltype(test(0.f))::value; // error
}
