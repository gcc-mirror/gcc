// PR c++/71368
// { dg-do compile { target c++20 } }

template <class T, class U> concept Same = __is_same_as(T,U);

struct inner;

template<typename X> concept CompoundReq = requires {
    // fine with concrete type in trailing type, i.e. inner& instead of X&
    { X::inner_member() } -> Same<X&>;
};

template<typename X> concept Concept = requires {
    { X::outer_member() } -> CompoundReq;
};

struct inner { static inner& inner_member(); };
struct outer { static inner outer_member(); };

int main()
{
    // fine
    static_assert( CompoundReq<inner> );
    static_assert( CompoundReq<decltype( outer::outer_member() )> );

    // ICE
    static_assert( Concept<outer> );
}
