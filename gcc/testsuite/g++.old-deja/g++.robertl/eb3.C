// Build don't link:
#include <complex>
template<class T>
class Vec {
public:
    Vec() { data = new T; }
    Vec<T> split() { Vec<T> tmp; operator=(tmp); return tmp; }
    void operator=(const Vec<T> &v) { data = new T; }
    T *data;
};
template class Vec<std::complex<double> >;
