// PR c++/53862
// { dg-options "-std=c++0x" }

typedef unsigned long size_t;

template<typename> struct is_scalar { static const bool value = true; };
template<bool, typename T> struct enable_if { typedef T type; };

template <size_t N, typename... Args>
void f(Args...) {}

template <size_t N, typename T, typename... Args>
typename enable_if<is_scalar<T>::value, void>::type f(T, Args...) {}

int main() {
    f<1>(1);
}
