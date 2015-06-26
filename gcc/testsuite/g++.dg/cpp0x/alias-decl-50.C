// PR c++/66067
// { dg-do compile { target c++11 } }

namespace std
{
  typedef int size_t;
    template < typename _Tp, _Tp > struct integral_constant
  {
    static constexpr _Tp value = 0;
    typedef integral_constant type;
  };
  typedef integral_constant < int, 0 > true_type;
  typedef integral_constant < int, 0 > false_type;
    template < typename _Tp > struct enable_if
  {
    typedef _Tp type;
  };
}
namespace meta
{
  inline namespace v1
  {
    template < template < typename ... >class, typename ... >struct defer;
      template < typename T > T * _nullptr_v (  );
      template < int N > using size_t = std::integral_constant < int, N >;
      template < int B > using bool_ = std::integral_constant < int, B >;
      template < typename T > using dec =
      std::integral_constant < decltype ( T::value ), 0 >;
      template < typename T > using eval = typename T::type;
      template < typename F, typename ... Args > using apply =
      typename F::template apply < Args ... >;
    namespace detail
    {
      template < typename > struct has_type_;
    }
    template < typename T > using has_type = eval < detail::has_type_ < T >>;
      template < typename T > struct id
    {
      using type = T;
    };
      template < template < typename ... >class > struct quote;
      template < typename > struct Trans_NS_extension_apply_list;
      template < typename, typename List > using apply_list =
      eval < Trans_NS_extension_apply_list < List >>;
    namespace detail
    {
      template < typename ... >struct _if_;
        template < typename If, typename Then > struct _if_ <If,
	Then >:std::enable_if < Then >
      {
      };
    }
    template < typename ... Args > using if_ =
      eval < detail::_if_ < Args ... >>;
      template < int If, typename ... Args > using if_c =
      eval < detail::_if_ < bool_ < If >, Args ... >>;
    namespace detail
    {
      template < typename ... >struct _and_:std::true_type
      {
      };
        template < typename ... >struct _or_:std::false_type
      {
      };
    }
    template < int >using not_c = bool_ < 0 >;
      template < typename Bool > using not_ = not_c < Bool::value >;
      template < typename ... >using and_ = eval < detail::_and_ <>>;
      template < typename > using or_ = eval < detail::_or_ <>>;
    namespace lazy
    {
      template < typename ... Bools > using and_ = defer < and_, Bools ... >;
    }
    template < typename ... Ts > struct list
    {
      static constexpr std::size_t size (  )
      {
	return sizeof ... ( Ts );
      }
    };
      template < typename List > using size = size_t < List::size (  ) >;
    namespace detail
    {
      template < typename > struct concat_;
    }
    template < typename ... Lists > using concat =
      eval < detail::concat_ < Lists ... >>;
      template < typename ListOfLists > using join =
      apply_list < quote < concat >, ListOfLists >;
    namespace detail
    {
      template < int >struct repeat_n_c_
      {
	using type = list <>;
      };
    }
    template < typename > using repeat_n = eval < detail::repeat_n_c_ < 0 >>;
      template < std::size_t N > using repeat_n_c =
      eval < detail::repeat_n_c_ < N >>;
    namespace detail
    {
      template < typename > struct at_impl_
      {
	template < typename T > static T eval ( T * );
      };
        template < typename, typename > struct at_;
        template < typename ... Ts, typename N > struct at_ <list < Ts ... >,
	N >:decltype ( at_impl_ < repeat_n <
		       N >>::eval ( _nullptr_v < id < Ts >> (  )... ) )
      {
      };
    }
    template < typename List, typename N > using at =
      eval < detail::at_ < List, N >>;
    template < typename List, std::size_t > using at_c =
      at < List, size_t < 0 >>;
    namespace detail
    {
      template < typename > struct back_;
        template < typename Head,
	typename ... List > struct back_ <list < Head, List ... >>
      {
	using type = at_c < list < Head >, sizeof ... ( List ) >;
      };
    }
    template < typename List > using back = eval < detail::back_ < List >>;
    namespace detail
    {
      template < typename, typename > struct push_front_;
        template < typename ... List,
	typename T > struct push_front_ <list < List ... >, T >
      {
	using type = list < T >;
      };
    }
    template < typename List, typename T > using push_front =
      eval < detail::push_front_ < List, T >>;
    namespace detail
    {
      template < typename > struct push_back_;
    }
    template < typename, typename T > using push_back =
      eval < detail::push_back_ < T >>;
    namespace detail
    {
      template < typename > struct transform_;
    }
    template < typename ... Args > using transform =
      eval < detail::transform_ < Args ... >>;
    namespace detail
    {
      template < typename > struct is_valid_;
        template < typename As, typename Ts > using substitutions_ =
	push_back < join < transform < concat < repeat_n_c < size < Ts >
      {
      }
      >>>>, list < back < As >>>;
      template < typename Ts > using substitutions =
	apply < if_c < size < Ts >
      {
      }
      , quote < substitutions_ >>>;
      template < typename > struct is_vararg_:std::false_type
      {
      };
      template < typename Tags > using is_variadic_ =
	is_vararg_ < at < push_front < Tags, void >, dec < size < Tags >>>>;
      template < typename Tags, int =
	is_variadic_ < Tags >::value > struct lambda_;
      template < typename ... As > struct lambda_ <list < As ... >, false >
      {
	using Tags = list < As ... >;
	using F = back < Tags >;
	  template < typename, typename > struct impl;
	  template < typename, typename > struct subst_;
	  template < template < typename ... >class C, typename ... Ts,
	  typename Args > struct subst_ <defer < C, Ts ... >, Args >
	{
	  using type = C < eval < impl < Ts, Args >> ... >;
	};
	  template < template < typename ... >class C, typename ... Ts,
	  typename Args > struct impl <defer < C, Ts ... >,
	  Args >:subst_ < defer < C >, Args >
	{
	};
	  template < typename ... Ts > using apply =
	  eval < if_c < sizeof ... ( Ts ), impl < F, list <>>>>;
      };
    }
    template < typename ... Ts > using lambda =
      if_c < sizeof ... ( Ts ), detail::lambda_ < list < Ts ... >>>;
    template < typename T > using is_valid = detail::is_valid_ < T >;
    namespace detail
    {
      template < typename ... >struct let_;
        template < typename Fn > struct let_ <Fn >
      {
	using type = apply < lambda < Fn >>;
      };
    }
    template < typename ... As > using let = eval < detail::let_ < As ... >>;
    template < typename > struct common_reference_base;
    template < typename ... >struct common_reference;
    namespace detail
    {
      template < typename > struct builtin_common_impl;
        template < typename U > using builtin_common_t =
	meta::apply < builtin_common_impl < U >>;
        template < typename, typename > using lazy_builtin_common_t =
	meta::defer < builtin_common_t >;
        template < typename > struct transform_reference;
        template < typename, typename U > using common_reference_base_ =
	common_reference_base < meta::eval < transform_reference < U >>>;
    }
    template < typename T, typename U > struct common_reference <T,
      U >:meta::if_ < meta::let < meta::lazy::and_ < meta::is_valid <
      detail::lazy_builtin_common_t < T, U >>,
      meta::or_ < meta::not_ < meta::has_type <
      detail::common_reference_base_ < T, U >>>>>>,
      detail::lazy_builtin_common_t < T, U >,
      detail::common_reference_base_ < T, U >>
    {
    };
  }
}
