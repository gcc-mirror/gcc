// { dg-do compile }
// { dg-options "-O2 -std=gnu++11 -fdump-tree-optimized" } */

#include <array>

// Return a copy of the underlying memory of an arbitrary value.
template <
    typename T,
    typename = typename std::enable_if<std::is_trivially_copyable<T>::value>::type
>
auto getMem(
    T const & value
) -> std::array<char, sizeof(T)> {
    auto ret = std::array<char, sizeof(T)>{};
    __builtin_memcpy(ret.data(), &value, sizeof(T));
    return ret;
}

template <
    typename T,
    typename = typename std::enable_if<std::is_trivially_copyable<T>::value>::type
>
auto fromMem(
    std::array<char, sizeof(T)> const & buf
) -> T {
    auto ret = T{};
    __builtin_memcpy(&ret, buf.data(), sizeof(T));
    return ret;
}

double foo1(std::uint64_t arg) {
    return fromMem<double>(getMem(arg));
}

double foo2(std::uint64_t arg) {
    return *reinterpret_cast<double*>(&arg);
}

double foo3(std::uint64_t arg) {
    double ret;
    __builtin_memcpy(&ret, &arg, sizeof(arg));
    return ret;
}

// { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "optimized" } }
