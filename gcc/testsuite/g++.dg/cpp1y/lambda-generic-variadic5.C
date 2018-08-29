// PR c++/65949
// { dg-do compile { target c++14 } }

#include <initializer_list>

template<class T, class... Ts>
struct Over : T, Over<Ts...>::type
{
    using type = Over;

    Over(T f1, Ts... f2)
        : T(f1), Over<Ts...>::type(f2...)
    {
    }

    using T::operator();
    using Over<Ts...>::type::operator();
};

template<class T>
struct Over<T> : T
{
    using type = T;
    using T::operator();
};

template <class... Lambdas>
auto CreateLambdas(Lambdas... lambdas)
{
    return Over<Lambdas...>(lambdas...);
}

int main()
{
    auto mesLambda = CreateLambdas
    (
        []()
        {

        },

        [](auto i)
        {
	  (void)i;
        },

        [](auto... args)
        {
            auto list = {args...};

            for(auto &&a : list)
	      (void)a;

            return 3;
        }
    );

    mesLambda();
    mesLambda(1);
    mesLambda(12,24,36,48);
}
