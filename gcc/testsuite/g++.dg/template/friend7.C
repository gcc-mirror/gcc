// { dg-do compile }

template <typename V>
struct b
{
  template <typename T>
  class a
  {
    template <typename>
    friend class a;

    T t_;

   public:
    a() {}
    a(a<T *> const &);
  };
};

template <typename V>
template <typename T>
b<V>::a<T>::a(a<T *> const &rhs): t_(*rhs.t_)
{}


int
f ()
{
  b<void *>::a<char *> q;
  b<void *>::a<char> w(q);

  return 0;
}
