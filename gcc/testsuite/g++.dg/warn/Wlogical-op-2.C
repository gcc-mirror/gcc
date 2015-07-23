// PR c++/66572
// { dg-do compile { target c++11 } }
// { dg-options "-Wlogical-op" }

struct false_type
{
    static constexpr bool value = false;
};

struct true_type
{
    static constexpr bool value = true;
};

template<typename T>
struct is_unsigned : false_type {};

template<>
struct is_unsigned<unsigned> : true_type {};

template<typename T1, typename T2>
bool foo()
{
    return is_unsigned<T1>::value && is_unsigned<T2>::value;
}

int main()
{
    foo<unsigned, unsigned>();
}
