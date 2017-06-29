typedef __PTRDIFF_TYPE__ ptrdiff_t;

template <typename T>
class I
{
public:
  typedef ptrdiff_t difference_type;
  I ();
  ~I ();
  I (T *);
  I (const I &);
  T &operator * ();
  T *operator -> ();
  T &operator [] (const difference_type &) const;
  I &operator = (const I &);
  I &operator ++ ();
  I operator ++ (int);
  I &operator -- ();
  I operator -- (int);
  I &operator += (const difference_type &);
  I &operator -= (const difference_type &);
  I operator + (const difference_type &) const;
  I operator - (const difference_type &) const;
  template <typename S> friend bool operator == (I<S> &, I<S> &);
  template <typename S> friend bool operator == (const I<S> &, const I<S> &);
  template <typename S> friend bool operator < (I<S> &, I<S> &);
  template <typename S> friend bool operator < (const I<S> &, const I<S> &);
  template <typename S> friend bool operator <= (I<S> &, I<S> &);
  template <typename S> friend bool operator <= (const I<S> &, const I<S> &);
  template <typename S> friend bool operator > (I<S> &, I<S> &);
  template <typename S> friend bool operator > (const I<S> &, const I<S> &);
  template <typename S> friend bool operator >= (I<S> &, I<S> &);
  template <typename S> friend bool operator >= (const I<S> &, const I<S> &);
  template <typename S> friend typename I<S>::difference_type operator - (I<S> &, I<S> &);
  template <typename S> friend typename I<S>::difference_type operator - (const I<S> &, const I<S> &);
  template <typename S> friend I<S> operator + (typename I<S>::difference_type , const I<S> &);
private:
  T *p;
};

template <typename T> bool operator == (I<T> &, I<T> &);
template <typename T> bool operator == (const I<T> &, const I<T> &);
template <typename T> bool operator != (I<T> &, I<T> &);
template <typename T> bool operator != (const I<T> &, const I<T> &);
template <typename T> bool operator < (I<T> &, I<T> &);
template <typename T> bool operator < (const I<T> &, const I<T> &);
template <typename T> bool operator <= (I<T> &, I<T> &);
template <typename T> bool operator <= (const I<T> &, const I<T> &);
template <typename T> bool operator > (I<T> &, I<T> &);
template <typename T> bool operator > (const I<T> &, const I<T> &);
template <typename T> bool operator >= (I<T> &, I<T> &);
template <typename T> bool operator >= (const I<T> &, const I<T> &);
template <typename T> typename I<T>::difference_type operator - (I<T> &, I<T> &);
template <typename T> typename I<T>::difference_type operator - (const I<T> &, const I<T> &);
template <typename T> I<T> operator + (typename I<T>::difference_type, const I<T> &);

ptrdiff_t foo (I<int> &);
I<int> &bar (I<int> &);
I<int> &baz (I<int> *);

void
f0 ()
{
  int i;
  const int j = 1;
  const int k = -1;
  const int m = 2;
  const int n = -7;
  int o = 1;
  int p = -1;
  #pragma omp for
  for (i = 0; i != 64; i += j)
    ;
  #pragma omp for
  for (i = 64; i != 0; i -= j)
    ;
  #pragma omp for
  for (i = 0; i != 64; i -= k)
    ;
  #pragma omp for
  for (i = 64; i != 0; i += k)
    ;
  #pragma omp for
  for (i = 0; i != 64; i += m)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 64; i != 0; i -= m)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 0; i != 64; i -= n)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 64; i != 0; i += n)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 0; i != 64; i += o)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 64; i != 0; i -= o)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 0; i != 64; i -= p)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = 64; i != 0; i += p)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
}

void
f1 (I<int> &x, I<int> &y, I<int> &u, I<int> &v)
{
  I<int> i, j;
  const int k = 1;
  const int l = -1;
  const int m = 2;
  const int n = -7;
  int o = 1;
  int p = -1;
  #pragma omp for
  for (i = x; i != y; i++)
    ;
  #pragma omp for
  for (i = x; y != i; ++i)
    ;
  #pragma omp for
  for (i = x; i != y; i = i + 1)
    ;
  #pragma omp for
  for (i = x; i != y; i = 1 + i)
    ;
  #pragma omp for
  for (i = y; i != x; i--)
    ;
  #pragma omp for
  for (i = y; x != i; --i)
    ;
  #pragma omp for
  for (i = y; i != x; i = i - 1)
    ;
  #pragma omp for
  for (i = y; i != x; i = -1 + i)
    ;
  #pragma omp for
  for (i = x; i != y; i = i + k)
    ;
  #pragma omp for
  for (i = x; i != y; i = k + i)
    ;
  #pragma omp for
  for (i = y; i != x; i = i - k)
    ;
  #pragma omp for
  for (i = y; i != x; i = -k + i)
    ;
  #pragma omp for
  for (i = x; i != y; i = i - l)
    ;
  #pragma omp for
  for (i = x; i != y; i = -l + i)
    ;
  #pragma omp for
  for (i = y; i != x; i = i + l)
    ;
  #pragma omp for
  for (i = y; i != x; i = l + i)
    ;
  #pragma omp for
  for (i = x; i != y; i = i + 2)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = 7 + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = i - 2)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = -7 + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = i + m)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = m + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = i - m)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = -m + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = i - n)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = -n + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = i + n)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = n + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = i + o)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = o + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = i - o)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = -o + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = i - p)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = x; i != y; i = -p + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = i + p)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
  #pragma omp for
  for (i = y; i != x; i = p + i)	// { dg-error "increment is not constant 1 or -1 for != condition" }
    ;
}
