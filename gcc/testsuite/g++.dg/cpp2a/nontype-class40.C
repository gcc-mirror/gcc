// PR c++/93083
// { dg-do compile { target c++20 } }

template<unsigned N>
struct FixedString
{
    char buf[N + 1]{};
    constexpr FixedString(char const* s) {
        for (unsigned i = 0; i != N; ++i) buf[i] = s[i];
    }

    auto operator<=>(const FixedString&) const = default;
    constexpr operator char const*() const { return buf; }
    constexpr static unsigned size() noexcept { return N; }
};

template<unsigned N> FixedString(char const (&)[N]) -> FixedString<N - 1>;

template <FixedString... names>
struct name_list
{
    template <FixedString name>
    using add_name = name_list<
        names...,
        FixedString<name.size()>{ name }
    >;
};


int main()
{
    using names =
        name_list<>
        ::add_name<"Zaphod Beeblebrox">;

}

// ----------------

template <int N> struct literal {
  constexpr literal(const char (&input)[N]) noexcept { }
  constexpr literal(const literal &) noexcept { }
};

template <literal Name, int id> struct field { };

template <literal Name> struct field<Name, 1u> { };

// ----------------

template <int N>
struct use_as_nttp {};

template <use_as_nttp Value>
struct has_nttp {};

template <use_as_nttp Value>
using has_nttp_2 = has_nttp<Value>;

// ----------------

using size_t = decltype(sizeof(0));

template <size_t N>
struct string_literal
{
  constexpr string_literal(const char*) {}
  string_literal(string_literal const&) = default;
};
template <size_t N>
string_literal(const char (&)[N]) -> string_literal<N - 1>;

template <string_literal Str>
struct type_string { };

template <string_literal Str>
void foo() {
  type_string<Str>{};
}
