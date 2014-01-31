// PR c++/57043
// { dg-do link }

template<typename D> struct complex { };

template<typename Tp>
complex<Tp>
pow(const complex<Tp>& x, const complex<Tp>& y) { return complex<Tp>(); }

template<typename T, typename U>
struct promote_2 { typedef T type; };

template<typename Tp, typename Up>
complex<typename promote_2<Tp, Up>::type>
pow(const complex<Tp>& x, const complex<Up>& y);

complex<double> (*powcc)(const complex<double>&, const complex<double>&) = pow;

int main() {}
