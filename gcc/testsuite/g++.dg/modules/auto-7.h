// PR c++/120644

enum class E { E0, E1 };

template <typename T>
constexpr auto fmt_kind = E::E0;

template <typename T>
class opt{};

template <typename T>
constexpr auto fmt_kind<opt<T>> = E::E1;
