// { dg-do compile { target c++11 } }

template<bool, class T = void>
struct enable_if {};

template<class T>
struct enable_if<true, T>
{
  using type = T;
};

template<class T>
struct is_true
{
  static constexpr bool value = true;
};

extern void* enabler;

template <typename T, typename enable_if<is_true<T>::value>::type*& = enabler>
class A
{
public:
    A()
    {}
    template <typename U>
    A& operator=( A<U>&& )
    {
        return *this;
    }
};

int main()
{
    A<int> a_i;
    A<double> a_d;

    a_i = a_d;  // { dg-error "cannot bind" }
}
