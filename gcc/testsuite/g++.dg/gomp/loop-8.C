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

extern I<int> i, j;
#pragma omp threadprivate (i, j)
extern I<int> k, l;
#pragma omp threadprivate (k, l)
I<int> k, l;

void
f1 (I<int> &x, I<int> &y)
{
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)	// { dg-error "expected iteration declaration or initialization" }
    for (j = x; j < y; j++)
      ;
}

void
f2 (I<int> &x, I<int> &y)
{
  #pragma omp for collapse(2)
  for (k = x; k < y; k++)	// { dg-error "expected iteration declaration or initialization" }
    for (l = x; l < y; l++)
      ;
}

template <int N>
void
f3 (I<int> &x, I<int> &y)
{
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)	// { dg-error "'i' is predetermined 'threadprivate' for 'private'" }
    for (j = x; j < y; j++)	// { dg-error "'j' is predetermined 'threadprivate' for 'private'" }
      ;
}

template <int N>
void
f4 (I<int> &x, I<int> &y)
{
  #pragma omp for collapse(2)
  for (k = x; k < y; k++)	// { dg-error "'k' is predetermined 'threadprivate' for 'private'" }
    for (l = x; l < y; l++)	// { dg-error "'l' is predetermined 'threadprivate' for 'private'" }
      ;
}

template <typename T>
void
f5 (I<T> &x, I<T> &y)
{
  #pragma omp for collapse(2)	// { dg-error "expected iteration declaration or initialization" }
  for (i = x; i < y; i++)	// { dg-error "'i' is predetermined 'threadprivate' for 'private'" }
    for (j = x; j < y; j++)	// { dg-error "'j' is predetermined 'threadprivate' for 'private'" }
      ;
}

template <typename T>
void
f6 (I<T> &x, I<T> &y)
{
  #pragma omp for collapse(2)	// { dg-error "expected iteration declaration or initialization" }
  for (k = x; k < y; k++)	// { dg-error "'k' is predetermined 'threadprivate' for 'private'" }
    for (l = x; l < y; l++)	// { dg-error "'l' is predetermined 'threadprivate' for 'private'" }
      ;
}

void
test (I<int> &x, I<int> &y)
{
  f3<0> (x, y);
  f4<0> (x, y);
  f5 (x, y);
  f6 (x, y);
}
