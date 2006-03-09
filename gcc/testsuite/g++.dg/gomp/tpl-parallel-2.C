// { dg-do compile }

template<typename T>
struct S
{
  T n;
  void test();
  void work();
};

template<typename T>
void S<T>::test()
{
  #pragma omp parallel num_threads(n)	// { dg-error "must be integral" }
    work();
}

template struct S<int>;
template struct S<long>;
template struct S<float>;	// { dg-error "instantiated from here" }
