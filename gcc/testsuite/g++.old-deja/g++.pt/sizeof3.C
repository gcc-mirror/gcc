// { dg-do run  }
// Adapted from testcase by Oskar Enoksson <osken393@student.liu.se>

extern "C" void abort();

template<class T0>
class A {
public:
  typedef T0 T;
};

template<int K>
class B {
  typedef A<char[K]> BC;
};

template<int N, int M>
class C { 
public:
  typedef A<char[M]> AC;
};

int main() {
  if (sizeof(C<3,7>::AC::T) != 7) 
    abort();
}
