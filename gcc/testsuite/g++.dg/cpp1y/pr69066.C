// PR c++/69066
// { dg-do compile { target c++14 } }

template <typename T> T&& declval();

template<typename T, T v>
struct integral_constant
{
  static constexpr T                value = v;
  typedef T                         value_type;
  typedef integral_constant<T, v>   type;
  constexpr operator value_type() const { return value; }
};

typedef integral_constant<bool, true>     true_type;
typedef integral_constant<bool, false>    false_type;

template <typename...>
using void_t = void;

template <typename, typename = void>
class is_zero_callable : public false_type
{
};

template <typename T>
class is_zero_callable<T, void_t<decltype(declval<T>()())>>
    : public true_type
{
};

template <typename TF, bool TLastStep>
struct curry_impl
{
    static auto exec(TF f)
    {
        // Bind `x` to subsequent calls.
        return [=](auto x)
        {
            auto bound_f = [=](auto... xs) -> decltype(f(x, xs...))
            {
                return f(x, xs...);
            };

            // Recursive step.
            return curry_impl<decltype(bound_f),
                is_zero_callable<decltype(bound_f)>{}>::exec(bound_f);
        };
    }
};

template <typename TF>
struct curry_impl<TF, true>
{
    static auto exec(TF f)
    {
        return f();
    }
};

template <typename TF>
auto curry(TF f)
{
    return curry_impl<TF, is_zero_callable<decltype(f)>{}>::exec(f);
}

int main()
{
    auto sum = [](int x, int y)
    {
        return x + y;
    };

    (void)curry(sum)(1)(1);
}
