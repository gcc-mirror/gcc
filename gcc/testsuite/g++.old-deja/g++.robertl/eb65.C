#include <iterator>

template<size_t n, size_t i> struct PartialDotProduct {
    template<class T>
    static T Expand(T* a, T* b) { return T(); }
};

const int N = 10;

template<class In1, class In2>
typename iterator_traits<In1>::value_type
dot(In1 f1, In2 f2)
{
    return PartialDotProduct<N, 0>::Expand(f1, f2);     // line 14
}

int main()
{
    double a[N], b[N];

    double s = dot(&a[0], &b[0]);
}
