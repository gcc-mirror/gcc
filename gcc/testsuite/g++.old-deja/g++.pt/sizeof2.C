// Although template class B is not used at all, it causes the
// incorrect specialization of A to be selected

// Adapted from testcase by Oskar Enoksson <osken393@student.liu.se>

extern "C" void abort();

template<int N, class T> // Base class
class A { public: static int n() { return sizeof(T); } };

template<int N> // Derived #1
class B: public A<N,char[N]> {};

template<int N, int M> // Derived #2 (wrong!)
class C: public A<N,char[M]> {};

int main() {
  if (C<1,2>::n() != 2)
    abort();
}
