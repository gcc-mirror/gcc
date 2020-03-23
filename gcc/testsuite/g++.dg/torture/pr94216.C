// { dg-do compile }
// { dg-additional-options "-g" }

template <int _Nm> struct A { typedef int _Type[_Nm]; };
template <int _Nm> struct B {
    typename A<_Nm>::_Type _M_elems;
    void operator[](int) { int a = *_M_elems; }
};
class C {
    struct D {
	using type = int *;
    };

public:
    using pointer = D::type;
};
class F {
public:
    using pointer = C::pointer;
    F(pointer);
};
struct G {
    int data;
};
template <int MaxDimensions> struct H {
    using dimensions_t = B<MaxDimensions>;
    dimensions_t dimensions;
    G mem;
};
template <int MaxDimensions, typename Allocator, typename DimT, typename AlignT>
H<MaxDimensions> alloc_view(int, DimT, AlignT, Allocator) {
    H<MaxDimensions> b;
    b.dimensions[0];
    return b;
}
namespace memory {
    template <typename> using DynMdView = H<6>;
}
class I {
    I();
    memory::DynMdView<void> m_view;
    F m_memory;
};
int c, d, e;
I::I() : m_view(alloc_view<6>(c, d, e, [] {})), m_memory(&m_view.mem.data) {}
