// { dg-do compile { target c++20 } }

template<class T>
struct X
{
    T first{};

    X& operator=(const X&) = delete;
    X& operator=(const X&) requires requires (T& t) { t = t; } { return *this; }
};

// C++20 std::pair<const int, int>:
using cxx20_pair = X<const int>;
static_assert( __is_trivially_constructible(cxx20_pair, const cxx20_pair&), "" );
static_assert( !__is_assignable(cxx20_pair&, const cxx20_pair&), "" );
static_assert( __is_trivially_copyable(cxx20_pair), "" );

template<bool, typename, typename F> struct conditional { using type = F; };
template<typename T, typename F> struct conditional<true, T, F> { using type = T; };

struct base
{
    base() = default;
    ~base() = default;
    base(const base&) = default;
    base& operator=(const base&) = delete;
};

struct nope;

template<class T>
struct Y : base
{
    T first{};

    Y& operator=(typename conditional<__is_assignable(T&, const T&), const Y&, const nope&>::type)
    { return *this; }
};

// C++17 std::pair<const int, int>:
using cxx17_pair = Y<const int>;
static_assert( __is_trivially_constructible(cxx17_pair, const cxx17_pair&), "" );
static_assert( ! __is_assignable(cxx17_pair&, const cxx17_pair&), "" );
static_assert( __is_trivially_copyable(cxx17_pair), "???" );
