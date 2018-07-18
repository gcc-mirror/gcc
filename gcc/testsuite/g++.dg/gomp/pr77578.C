// PR c++/77578
// { dg-do compile }

template <typename T>
class A 
{
};

template <typename T>
struct B
{
};

template <typename T>
struct B <A <T> >
{
  typedef A <T> C;
  typedef typename C::D D;
 
  template <typename U>
  static void
  foo (const D x, const D y)
  {
    U u;
    {
      #pragma omp parallel for 
      for (u.bar().y() = x.y(); u.bar().y() <= y.y(); u.bar().y()++) // { dg-error "expected" }
	;
    }
  }
};
