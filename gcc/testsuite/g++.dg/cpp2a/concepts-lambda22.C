// PR c++/118698
// { dg-do compile { target c++20 } }

template <typename T> struct foo {};
template <typename T> struct bar {};

template <class T> T&& declval ();

template <typename T, typename U>
concept callable = requires { declval<T>()(declval<U>()); };

template <typename T, template <typename...> typename U>
concept is_specialization_of = callable<decltype([]<typename... Args>( U<Args...> const& ) { }),T>;

static_assert( is_specialization_of<foo<int>,foo> == true );
static_assert( is_specialization_of<foo<int>,bar> == false );

template <typename T> concept is_foo = is_specialization_of<T,foo>;

static_assert(  is_foo<foo<int>> );
static_assert(  is_foo<bar<int>> == false );
