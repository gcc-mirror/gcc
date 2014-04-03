// { dg-do compile }
template <typename _InputIterator> void __distance (_InputIterator);
template <typename _InputIterator>
void distance (_InputIterator, _InputIterator p2)
{
  __distance (p2);
}

namespace boost
{
template <class Iterator> struct A
{
  typedef typename Iterator::difference_type type;
};
template <class T> typename T::const_iterator end (T &);
template <class T> typename T::const_iterator begin (T &);
template <class T> struct D : A<typename T::const_iterator>
{
};
template <class T> typename D<T>::type distance (const T &p1)
{
  distance (boost::begin (p1), boost::end (p1));
  return 0;
}
template <class IteratorT> class B
{
public:
  typedef B type;
  typedef IteratorT const_iterator;
};
}

typedef int storage_t[];
struct F;
template <template <typename> class> struct G
{
  G (const G &p1) { p1.m_fn1 ().m_fn1 (0); }
  const F &m_fn1 () const
  {
    const void *a;
    a = &data_m;
    return *static_cast<const F *>(a);
  }
  storage_t *data_m;
};

struct F
{
  virtual F *m_fn1 (void *) const;
};
template <typename> struct H;
struct C : G<H>
{
  typedef int difference_type;
};
boost::B<C> AllTransVideos ();
int b = boost::distance (AllTransVideos ());

