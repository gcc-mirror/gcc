// PR c++/85695
// { dg-do compile { target c++17 } }

template <typename T, T v>
struct integral_constant {
    using value_type = T;
    static constexpr const value_type value = v;
    constexpr operator value_type (void) const { return value; }
};
template <typename T> struct is_trivial
    : public integral_constant<bool, __is_trivial(T)> {};

template <typename T>
T clone_object (const T& p)
{
    if constexpr (is_trivial<T>::value)
        return p;
    else
        return p.clone();
}
int main (void) { return clone_object(0); }
