// PR c++/67240
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-diagnostics-depth=2" }

template <class T, class U> concept Same = __is_same_as(T,U);

int foo(int x)
{
    return x;
}

template <typename T>
concept C1 = requires (T x) {
    {foo(x)} -> Same<int&>; // { dg-error "placeholder constraints" }
};

template <typename T>
concept C2 = requires (T x) {
    {foo(x)} -> Same<void>; // { dg-error "placeholder constraints" }
};

static_assert( C1<int> );	// { dg-error "assert" }
static_assert( C2<int> );	// { dg-error "assert" }
