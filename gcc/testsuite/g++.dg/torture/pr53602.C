// { dg-do compile }
// { dg-options "-std=c++11" }

namespace std
{
  template <class, class>
  struct pair
  {
  };
  struct input_iterator_tag
  {
  };
  struct forward_iterator_tag : public input_iterator_tag
  {
  };
  template <typename, typename _Tp, typename = _Tp>
  struct iterator
  {
  };
}
namespace __gnu_cxx
{
  template <typename _Tp>
  struct new_allocator
  {
    typedef _Tp pointer;
    typedef _Tp value_type;
    template <typename _Tp1>
    struct rebind
    {
      typedef new_allocator <_Tp1> other;
    };
  };
}
namespace std
{
  template <typename _Tp>
  struct allocator : public __gnu_cxx::new_allocator <_Tp>
  {
  };
}
extern "C"
{
  struct rtl_String;
  void rtl_string_release (rtl_String *) throw ();
  void rtl_string_newFromStr (rtl_String * *, const char *) throw ();
}
namespace std
{
  template <typename, typename, typename> struct binary_function;
  template <typename _Tp>
  struct equal_to : public binary_function <_Tp, _Tp, bool>
  {
  };
}
namespace rtl
{
  struct OString
  {
    rtl_String * pData;
    OString (const char *value)
    {
      rtl_string_newFromStr (&pData, value);
    }
     ~OString ()
    {
      rtl_string_release (pData);
    }
  };
  struct OStringHash;
}
namespace boost
{
  template <class> struct hash;
  namespace unordered
  {
    template <class T, class = boost::hash <T>, class = std::equal_to <T>, class = std::allocator <T>>class unordered_set;
  }
  using boost::unordered::unordered_set;
  namespace detail
  {
    template <bool>
    struct if_true
    {
      template <class, class F>
      struct then
      {
	typedef F type;
      };
    };
  }
  template <class, class> struct pointer_to_other;
  template <class T, class U>
  struct pointer_to_other <T *, U>
  {
    typedef U type;
  };
  namespace unordered
  {
    namespace detail
    {
      template <typename T, T> struct integral_constant
      {
      };
      struct choice9
      {
	typedef char (&type)[9];
      };
      struct choice8:choice9
      {
      };
      struct choice7:choice8
      {
      };
      struct choice6:choice7
      {
      };
      struct choice5:choice6
      {
      };
      struct choice4:choice5
      {
      };
      struct choice3:choice4
      {
      };
      struct choice2:choice3
      {
      };
      struct choice1:choice2
      {
      };
      choice1 choose ();
      template <typename Alloc, typename T>
      struct rebind_wrap
      {
	typedef typename Alloc::template rebind <T>::other type;
      };
      template <typename, typename T2>
      struct sfinae:T2
      {
      };
      template <typename Tp, typename Default>
      struct default_type_pointer
      {
	template <typename X>
	static boost::unordered::detail::sfinae <typename X::pointer, choice1> test (choice1);
	struct DefaultWrap
	{
	  typedef Default pointer;
	};
	enum { value = (1 == sizeof (test <Tp> (choose ()))) };
	typedef typename boost::detail::if_true <value>::template then <Tp, DefaultWrap>::type::pointer type;
      };
      template <typename Tp, typename Default>
      struct default_type_const_pointer
      {
	template <typename>
	static choice2::type test (choice2);
	struct DefaultWrap
	{
	};
	enum { value = (1 == sizeof (test <Tp> (choose ()))) };
	typedef typename boost::detail::if_true <value>::template then <Tp, DefaultWrap> type;
      };
      struct default_type_propagate_on_container_swap
      {
	struct DefaultWrap
	{
	};
      };
      template <typename Alloc>
      struct allocator_traits
      {
	typedef typename Alloc::value_type value_type;
	typedef typename default_type_pointer <Alloc, value_type *>::type pointer;
	template <typename T>
	struct pointer_to_other : boost::pointer_to_other <pointer, T>
	{
	};
	typedef typename default_type_const_pointer <Alloc, typename pointer_to_other <value_type>::type>::type const_pointer;
      };
    }
    namespace detail
    {
      struct move_tag
      {
      };
      template <typename> struct table;
      template <typename NodeAlloc>
      struct node_constructor
      {
	void construct_value ()
	{
	}
      };
      struct ptr_bucket
      {
	ptr_bucket ()
	{
	}
      };
      template <typename A, typename Bucket, typename Node>
      struct buckets
      {
	typedef Node node;
	typedef Bucket bucket;
	typedef typename boost::unordered::detail::rebind_wrap <A, node>::type node_allocator;
	typedef typename boost::unordered::detail::rebind_wrap <A, bucket>::type bucket_allocator;
	typedef boost::unordered::detail::allocator_traits <node_allocator> node_allocator_traits;
	typedef boost::unordered::detail::allocator_traits <bucket_allocator> bucket_allocator_traits;
	typedef typename node_allocator_traits::pointer node_pointer;
	typedef typename node_allocator_traits::const_pointer const_node_pointer;
	typedef typename bucket_allocator_traits::pointer bucket_pointer;
	typedef boost::unordered::detail::node_constructor <node_allocator> node_constructor;
	bucket_pointer buckets_;
	unsigned size_;
	template <typename Types>
	buckets (boost::unordered::detail::table <Types>, boost::unordered::detail::move_tag) : buckets_ (), size_ ()
	{
	}
      };
      struct functions
      {
      };
    }
  }
  namespace detail
  {
    template <class Category, class T, class, class, class>
    struct iterator_base:std::iterator <Category, T>
    {
    };
  }
  template <class Category, class T, class Distance, class Pointer = T, class Reference = T>
  struct iterator:boost::detail::iterator_base <Category, T, Distance, Pointer, Reference>
  {
  };
  namespace unordered
  {
    namespace iterator_detail
    {
      template <typename, typename NodePointer, typename Value> struct c_iterator:public boost::iterator <std::forward_iterator_tag, Value, int>
      {
	friend bool operator== (c_iterator, c_iterator)
	{
	}
      };
    }
    namespace detail
    {
      template <typename ValueType>
      struct value_base
      {
	typedef ValueType value_type;
	value_type value ()
	{
	}
      };
      template <typename Types>
      struct table:boost::unordered::detail::buckets <typename Types::allocator, typename Types::bucket, typename Types::key_equal>
      {
	typedef typename Types::value_type value_type;
	typedef boost::unordered::detail::buckets <typename Types::allocator, typename Types::bucket, typename Types::node> buckets;
	typedef typename buckets::node_pointer node_pointer;
	typedef typename buckets::const_node_pointer const_node_pointer;
	typedef boost::unordered::iterator_detail::c_iterator <const_node_pointer, node_pointer, value_type> c_iterator;
	unsigned max_size ()
	{
	}
      };
      template <typename> struct table_impl;
      template <typename T>
      struct ptr_node : boost::unordered::detail::value_base <T>, boost::unordered::detail::ptr_bucket
      {
	boost::unordered::detail::ptr_bucket bucket_base;
	unsigned hash_;
	ptr_node () : bucket_base (), hash_ ()
	{
	}
      };
      template <typename A, typename T, typename, typename> struct pick_node2
      {
      };
      template <typename A, typename T> struct pick_node2 <A, T, boost::unordered::detail::ptr_node <T> *, boost::unordered::detail::ptr_bucket *>
      {
	typedef boost::unordered::detail::ptr_node <T> node;
	typedef boost::unordered::detail::ptr_bucket bucket;
      };
      template <typename A, typename T> struct pick_node
      {
	typedef boost::unordered::detail::allocator_traits <typename boost::unordered::detail::rebind_wrap <A, boost::unordered::detail::ptr_node <T>>::type> tentative_node_traits;
	typedef boost::unordered::detail::allocator_traits <typename boost::unordered::detail::rebind_wrap <A, boost::unordered::detail::ptr_bucket>::type> tentative_bucket_traits;
	typedef pick_node2 <A, T, typename tentative_node_traits::pointer, typename tentative_bucket_traits::pointer> pick;
	typedef typename pick::node node;
	typedef typename pick::bucket bucket;
      };
      template <typename A, typename T, typename H, typename P>
      struct set
      {
	typedef boost::unordered::detail::set <A, T, H, P> types;
	typedef T value_type;
	typedef P key_equal;
	typedef typename boost::unordered::detail::rebind_wrap <A, value_type>::type allocator;
	typedef boost::unordered::detail::pick_node <allocator, value_type> pick;
	typedef typename pick::node node;
	typedef typename pick::bucket bucket;
	typedef boost::unordered::detail::table_impl <types> table;
      };
      template <typename Types>
      struct table_impl : boost::unordered::detail::table <Types>
      {
	typedef boost::unordered::detail::table <Types> table;
	typedef typename table::node_constructor node_constructor;
	table_impl () : table ()
	{
	}
	template <class InputIt>
	void insert_range_impl2 (node_constructor, InputIt)
	{
	}
      };
    }
    template <class T, class H, class P, class A>
    struct unordered_set
    {
      typedef T key_type;
      typedef T value_type;
      typedef boost::unordered::detail::set <A, T, H, P> types;
      typedef typename types::table table;
      typedef typename table::c_iterator const_iterator;
      typedef typename table::c_iterator iterator;
      table table_;
      bool empty ()
      {
	return table_.size_;
      }
      iterator end ()
      {
      }
      std::pair <iterator, bool> insert (value_type)
      {
      }
      unsigned erase (const key_type &);
      const_iterator find (const key_type);
    };
    template <class T, class H, class P, class A>
    unsigned unordered_set <T, H, P, A>::erase (const key_type &)
    {
    }
  }
}
using namespace::rtl;
namespace skeletonmaker
{
  void
  checkDefaultInterfaces (boost::unordered_set <OString, OStringHash> interfaces,
			  boost::unordered_set <OStringHash> services, OString)
  {
    if (services.empty ())
      interfaces.erase ("com.sun.star.lang.XServiceInfo");
    else if (interfaces.find ("com.sun.star.lang.XServiceInfo") == interfaces.end ())
      interfaces.insert ("com.sun.star.lang.XServiceInfo");
  }
}
