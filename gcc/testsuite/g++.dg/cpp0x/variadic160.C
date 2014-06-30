// PR c++/61539
// { dg-do compile { target c++11 } }

template <typename _CharT> class A;
template <typename> class B;
template <class charT> class C;
template <> class C<char>
{
  virtual void xparse (int &, const B<A<char> > &) const;
};
template <class T, class charT = char> class G : C<charT>
{
public:
  G (void *) {}
  void default_value (const T &);
  void xparse (int &, const B<A<charT> > &) const;
};
template <class T, class charT>
void validate (int &, const B<A<charT> > &, T *, int);
template <class T, class charT>
void G<T, charT>::xparse (int &p1, const B<A<charT> > &p2) const
{
  validate (p1, p2, (T *)0, 0);
}
template <class T> G<T> *value (T *) { return new G<T>(0); }
namespace Eigen
{
template <typename T> struct D;
template <typename, int, int, int = 0, int = 0, int = 0 > class F;
template <typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
          int _MaxCols>
struct D<F<_Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols> >
{
  typedef _Scalar Scalar;
};
template <typename, int, int, int, int, int _MaxCols> class F
{
public:
  typedef typename Eigen::D<F>::Scalar Scalar;
  F (const Scalar &, const Scalar &, const Scalar &);
};
template <class... T>
void validate (int &, const B<A<char> > &, Eigen::F<T...> *);
}
int main (int, char *[])
{
  Eigen::F<double, 3, 1> a (0, 0, 0);
  value (&a)->default_value (Eigen::F<double, 3, 1>(0, 0, 0));
}
