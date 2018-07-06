/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-Wno-return-type" } */

namespace std
{
  template < class _T1, class _T2 > struct pair
  {
  };
}
extern "C"
{
  extern "C"
  {
    typedef int int32_t __attribute__ ((__mode__ (__SI__)));
    struct _pthread_fastlock
    {
    }
    pthread_mutexattr_t;
  }
}
namespace std
{
  struct __numeric_limits_base
  {
  };
    template < typename _Tp >
    struct numeric_limits:public __numeric_limits_base
  {
    static const bool is_integer = true;
  };
};
typedef unsigned int uint32_t;
namespace std
{
  template < typename _Alloc > class allocator;
  template < class _CharT > struct char_traits;
    template < typename _CharT, typename _Traits =
    char_traits < _CharT >, typename _Alloc =
    allocator < _CharT > >class basic_string;
  typedef basic_string < char >string;
}
namespace __gnu_cxx
{
  template < typename _Tp > class new_allocator
  {
  };
}
namespace std
{
  template < typename _Tp > class allocator:public __gnu_cxx::new_allocator <
    _Tp >
  {
  };
  template < typename _CharT, typename _Traits,
    typename _Alloc > class basic_string
  {
  public:inline basic_string ();
    basic_string (const _CharT * __s, const _Alloc & __a = _Alloc ());
  };
}
namespace boost
{
  template < class T > class integer_traits:public std::numeric_limits < T >
  {
  };
  namespace detail
  {
    template < class T, T min_val, T max_val > class integer_traits_base
    {
    };
  }
  template <> class integer_traits < int >:public std::numeric_limits < int >,
    public detail::integer_traits_base < int, (-2147483647 - 1), 2147483647 >
  {
  };
  namespace random
  {
    template < class IntType, IntType m > class const_mod
    {
    public:static IntType add (IntType x, IntType c)
      {
      }
      static IntType mult (IntType a, IntType x)
      {
	return mult_schrage (a, x);
      }
      static IntType mult_add (IntType a, IntType x, IntType c)
      {
	return add (mult (a, x), c);
      }
      static IntType mult_schrage (IntType a, IntType value)
      {
	for (;;)
	  {
	    if (value > 0)
	      break;
	    value += m;
	  }
      }
    };
    template < class IntType, IntType a, IntType c, IntType m,
      IntType val > class linear_congruential
    {
    public:typedef IntType result_type;
      static const IntType modulus = m;
    explicit linear_congruential (IntType x0 = 1):_modulus (modulus),
	_x (_modulus ? (x0 % _modulus) :
	    x0)
      {
      }
      IntType operator    () ()
      {
	_x = const_mod < IntType, m >::mult_add (a, _x, c);
      }
    private:IntType _modulus;
      IntType _x;
    };
  }
  typedef random::linear_congruential < int32_t, 16807, 0, 2147483647,
    1043618065 > minstd_rand0;
  namespace random
  {
    namespace detail
    {
      template < class T > struct ptr_helper
      {
	typedef T value_type;
	typedef T & reference_type;
	typedef const T & rvalue_type;
	static reference_type ref (T & r)
	{
	}
      };
        template < class T > struct ptr_helper <T & >
      {
	typedef T value_type;
	typedef T & rvalue_type;
      };
    }
  }
  template < class UniformRandomNumberGenerator, class RealType =
    double >class uniform_01
  {
  public:typedef UniformRandomNumberGenerator base_type;
    typedef RealType result_type;
    explicit uniform_01 (base_type rng):_rng (rng),
      _factor (result_type (1) /
	       (result_type ((_rng.max) () - (_rng.min) ()) +
		result_type (std::numeric_limits <
			     base_result >::is_integer ? 1 : 0)))
    {
    }
    result_type operator    () ()
    {
      return result_type (_rng () - (_rng.min) ()) * _factor;
    }
  private:typedef typename base_type::result_type base_result;
    base_type _rng;
    result_type _factor;
  };
  namespace random
  {
    namespace detail
    {
      template < class UniformRandomNumberGenerator >
	class pass_through_engine
      {
      private:typedef ptr_helper < UniformRandomNumberGenerator >
	  helper_type;
      public:typedef typename helper_type::value_type base_type;
	typedef typename base_type::result_type result_type;
	explicit pass_through_engine (UniformRandomNumberGenerator
				      rng):_rng (static_cast <
						 typename helper_type::
						 rvalue_type > (rng))
	{
	}
	result_type min () const
	{
	}
	result_type max () const
	{
	}
	base_type & base ()
	{
	}
	result_type operator    () ()
	{
	  return base ()();
	}
      private:UniformRandomNumberGenerator _rng;
      };
    }
    template < class RealType, int w, unsigned int p,
      unsigned int q > class lagged_fibonacci_01
    {
    public:typedef RealType result_type;
      static const unsigned int long_lag = p;
        lagged_fibonacci_01 ()
      {
	seed ();
      }
    public:void seed (uint32_t value = 331u)
      {
	minstd_rand0 intgen (value);
	seed (intgen);
      }
      template < class Generator > void seed (Generator & gen)
      {
	typedef detail::pass_through_engine < Generator & >ref_gen;
	uniform_01 < ref_gen, RealType > gen01 =
	  uniform_01 < ref_gen, RealType > (ref_gen (gen));
	for (unsigned int j = 0; j < long_lag; ++j)
	  x[j] = gen01 ();
      }
      RealType x[long_lag];
    };
  }
  typedef random::lagged_fibonacci_01 < double, 48, 607,
    273 > lagged_fibonacci607;
  namespace random
  {
    namespace detail
    {
      template < bool have_int, bool want_int > struct engine_helper;
        template <> struct engine_helper <true, true >
      {
	template < class Engine, class DistInputType > struct impl
	{
	  typedef pass_through_engine < Engine > type;
	};
      };
    }
  }
  template < class Engine, class Distribution > class variate_generator
  {
  private:typedef random::detail::pass_through_engine < Engine >
      decorated_engine;
  public:typedef typename decorated_engine::base_type engine_value_type;
    typedef Distribution distribution_type;
  variate_generator (Engine e, Distribution d):_eng (decorated_engine (e)),
      _dist (d)
    {
    }
  private:enum
    {
      have_int =
	std::numeric_limits <
	typename decorated_engine::result_type >::is_integer, want_int =
	std::numeric_limits < typename Distribution::input_type >::is_integer
    };
    typedef typename random::detail::engine_helper < have_int,
      want_int >::template impl < decorated_engine,
      typename Distribution::input_type >::type internal_engine_type;
    internal_engine_type _eng;
    distribution_type _dist;
  };
  template < class RealType = double >class uniform_real
  {
  public:typedef RealType input_type;
  };
}
namespace alps
{
  class BufferedRandomNumberGeneratorBase
  {
  };
    template < class RNG >
    class BufferedRandomNumberGenerator:public
    BufferedRandomNumberGeneratorBase
  {
  public: BufferedRandomNumberGenerator ():rng_ (), gen_ (rng_,
		   boost::
		   uniform_real <> ())
    {
    }
  protected:  RNG rng_;
    boost::variate_generator < RNG &, boost::uniform_real <> >gen_;
  };
}
namespace boost
{
  namespace detail
  {
    class sp_counted_base
    {
    };
    class shared_count
    {
    private:sp_counted_base * pi_;
    public:shared_count ():pi_ (0)
      {
      }
      template < class Y > explicit shared_count (Y * p):pi_ (0)
      {
      }
    };
  }
  template < class T > class shared_ptr
  {
  public:typedef T element_type;
  template < class Y > explicit shared_ptr (Y * p):px (p), pn (p)
    {
    }
    T *px;
    detail::shared_count pn;
  };
}
namespace std
{
  template < typename _Key, typename _Tp, typename _Compare =
    std::allocator < std::pair < const _Key, _Tp > > > class map
  {
  public:typedef _Key key_type;
    typedef _Tp mapped_type;
      mapped_type & operator[] (const key_type & __k)
    {
    }
  };
}
namespace alps
{
  namespace detail
  {
    template < class BASE > class abstract_creator
    {
    public:typedef BASE base_type;
      virtual base_type *create () const = 0;
    };
      template < class BASE,
      class T > class creator:public abstract_creator < BASE >
    {
    public:typedef BASE base_type;
      base_type *create () const
      {
	return new T ();
      }
    };
  }
  template < class KEY, class BASE > class factory
  {
  public:typedef BASE base_type;
    typedef KEY key_type;
    typedef boost::shared_ptr < detail::abstract_creator < base_type >
      >pointer_type;
    template < class T > bool register_type (key_type k)
    {
      creators_[k] = pointer_type (new detail::creator < BASE, T > ());
    }
  private:typedef std::map < key_type, pointer_type > map_type;
    map_type creators_;
  };
  class RNGFactory:public factory < std::string,
    BufferedRandomNumberGeneratorBase >
  {
  public:RNGFactory ();
  };
}
alps::RNGFactory::RNGFactory ()
{
  register_type < BufferedRandomNumberGenerator < boost::lagged_fibonacci607 >
    >("lagged_fibonacci607");
}
