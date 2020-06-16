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

void
f1 (I<int> &x, I<int> &y)
{
  I<int> i;
  #pragma omp for collapse(2)
  for (i = x; i < y; i++)	// { dg-error "the same loop iteration variables 'i' used in multiple associated loops" }
    for (i = x; i < y; i++)
      ;
  #pragma omp for collapse(2)
  for (I<int> j = x; j < y; j++)// { dg-error "the same loop iteration variables 'j' used in multiple associated loops" }
    for (j = y; j > x; j--)
      ;
}
