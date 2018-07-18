// { dg-options "-std=c++17 -fconcepts" }

struct inner;

template<typename X> concept bool CompoundReq = requires {
    // fine with concrete type in trailing type, i.e. inner& instead of X&
    { X::inner_member() } -> X&;
};

template<typename X> concept bool Concept = requires {
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
