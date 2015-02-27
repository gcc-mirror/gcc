/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto -O2 -g}} } */

void frexp (int, int *);
namespace std
{
  int ldexp (int, int);
  struct A
    {
    };
  template <class T> T get_min_shift_value ();
  template <class> struct min_shift_initializer
    {
      struct B
	{
	  B () { get_min_shift_value<long double> (); }
	} static const b;
      static void
	  m_fn1 ()
	    {
	      b;
	    }
    };
  template <class T>
      const typename min_shift_initializer<T>::B min_shift_initializer<T>::b;
  template <class T>
      inline T
      get_min_shift_value ()
	{
	  using std::ldexp;
	  static T c = ldexp (0, 0);
	  min_shift_initializer<T>::m_fn1;
	}
  template <class T, class Policy>
      void
      float_next_imp (T p1, Policy p2)
	{
	  using std::ldexp;
	  int d;
	  float_next (0, p2);
	  frexp (p1, &d);
	}
  template <class T, class Policy>
      int
      float_next (const T &p1, Policy &p2)
	{
	  float_next_imp (p1, p2);
	}
  template <class T, class Policy> void float_prior_imp (T, Policy)
    {
      get_min_shift_value<T> ();
    }
  template <class T, class Policy> int float_prior (T, Policy)
    {
      float_prior_imp (static_cast<T> (0), 0);
    }
  template <class T, class U, class Policy>
      void
      nextafter (T p1, U p2, Policy p3)
	{
	  p2 ? float_next (0, p3) : float_prior (p1, 0);
	}
  long double e;
  int f;
  void
      nextafter ()
	{
	  nextafter (e, f, A ());
	}
}
