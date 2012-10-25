// { dg-require-effective-target tls }

template <class T> struct B
{
  T t;
};

class A {
    static B<int> b;
#pragma omp threadprivate(b)
};

B<int> A::b;
