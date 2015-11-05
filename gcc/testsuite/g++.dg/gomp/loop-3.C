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
f1 (I<int> &x, I<int> &y, I<int> &u, I<int> &v)
{
  I<int> i, j;
  #pragma omp for
  for (i = x; i < y; i++)
    ;
  #pragma omp for
  for (i = x; y > i; i++)
    ;
  #pragma omp for
  for (i = x; i < y; i = i + 2)
    ;
  #pragma omp for
  for (i = x; i < y; i = 2 + i)
    ;
  #pragma omp for
  for (i = i; i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = i + 3; i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = bar (i); i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = baz (&i); i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; i <= i + 5; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; i <= baz (&i); i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; baz (&i) > i; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; i <= i; i++) /* { dg-error "invalid controlling predicate|condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; i < y; i += foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; i < y; i = i + foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = x; i < y; i = foo (i) + i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i = i + 2)
    for (j = u; j < y; j += 2)
      ;
  #pragma omp for collapse(2)
  for (i = j; i < y; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    for (j = x; j < y; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    for (j = i; j < v; j += 2)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i = i + 2)
    for (j = i + 3; j < v; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)
    for (j = baz (&i); j < v; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = v; j > i; j--)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = x; j < i; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = u; j < i + 4; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < j + 4; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < j; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < bar (j); i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = u; j < baz (&i); j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i += foo (j)) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)
    for (j = u; j < v; j += foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i = foo (j) + i) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)
    for (j = u; j < y; j = j + (i - v)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i = foo (j) + i) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)
    for (j = u; j < v; j = j + foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
}

void
f2 (I<int> &x, I<int> &y, I<int> &u, I<int> &v)
{
  #pragma omp for
  for (I<int> i = x; i < y; i++)
    ;
  #pragma omp for
  for (I<int> i = x; y > i; i++)
    ;
  #pragma omp for
  for (I<int> i = x; i < y; i = i + 2)
    ;
  #pragma omp for
  for (I<int> i = x; i < y; i = 2 + i)
    ;
  #pragma omp for
  for (I<int> i = i; i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = i + 3; i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = bar (i); i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = baz (&i); i < y; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; i <= i + 5; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; i <= baz (&i); i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; baz (&i) > i; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; i <= i; i++) /* { dg-error "invalid controlling predicate|condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; i < y; i += foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; i < y; i = i + foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (I<int> i = x; i < y; i = foo (i) + i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i = i + 2)
    for (I<int> j = u; j < y; j += 2)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    for (I<int> j = i; j < v; j += 2)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i = i + 2)
    for (I<int> j = i + 3; j < v; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++)
    for (I<int> j = baz (&i); j < v; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (I<int> j = v; j > i; j--)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (I<int> j = x; j < i; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (I<int> j = u; j < i + 4; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (I<int> j = u; j < baz (&i); j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++)
    for (I<int> j = u; j < v; j += foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++)
    for (I<int> j = u; j < y; j = j + (i - v)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i++)
    for (I<int> j = u; j < v; j = j + foo (i)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
}

void
f3 (I<int> &x, I<int> &y, I<int> &u, I<int> &v)
{
  I<int> j;
  #pragma omp for collapse(2)
  for (I<int> i = j; i < y; i = i + 2)
    for (I<int> j = x; j < y; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < j + 4; i++)
    for (I<int> j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < j; i++)
    for (I<int> j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < bar (j); i++)
    for (I<int> j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i += foo (j))
    for (I<int> j = u; j < v; j++)
      ;
  #pragma omp for collapse(2)
  for (I<int> i = x; i < y; i = foo (j) + i)
    for (I<int> j = u; j < v; j++)
      ;
}
