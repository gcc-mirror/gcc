// { dg-do compile }
// { dg-options "-O -fstrict-aliasing -ftree-pre -fno-tree-fre -fno-tree-sra" }

typedef __SIZE_TYPE__ size_t;
namespace std 
{
  template < class _T1, class > struct pair
  {
    _T1 first;
  };
}
namespace __gnu_cxx
{
  template < typename _Tp > class new_allocator
  {
  public:
    typedef size_t size_type;
    typedef _Tp * pointer;
    typedef _Tp const_pointer;
    typedef _Tp & reference;
    typedef const _Tp & const_reference;
    template < typename _Tp1 > struct rebind
    {
      typedef new_allocator < _Tp1 > other;
    };
  };
}
namespace std
{
template < typename _Tp > class allocator:
  public __gnu_cxx::new_allocator < _Tp >
  {};
  template < typename, typename, typename > struct binary_function;
  template < typename _Tp > struct less:binary_function < _Tp, _Tp, bool >
  {};
}
namespace __gnu_cxx
{
  namespace typelist
  {
    struct null_type;
    template < typename Root > struct node
    {
      typedef Root root;
    };
    template < typename, typename > struct chain;
    namespace detail
    {
      template < typename, int >struct chain_at_index_;
      template
	<
	typename
	Hd, typename Tl > struct chain_at_index_ <chain < Hd, Tl >, 0 >
      {
	typedef Hd type;
      };
      template
	<
	typename
	Hd, typename Tl, int i > struct chain_at_index_ <chain < Hd, Tl >, i >
      {
	typedef typename chain_at_index_ < Tl, i - 1 >::type type;
      };
    }
    template < typename Typelist, int i > struct at_index
    {
      typedef typename Typelist::root root_type;
      typedef detail::chain_at_index_ < root_type, i > index_type;
      typedef typename index_type::type type;
    };
    template < typename T1, typename T2 > struct create2
    {
      typedef node < chain < T1, chain < T2, null_type > > >type;
    };
  }
}
namespace std
{
  namespace tr1
  {
    template < typename _Tp, _Tp __v > struct integral_constant
    {
      static const _Tp value = __v;
    };
    typedef integral_constant < bool, false > false_type;
    template < typename, typename > struct is_same:false_type
    {};
  }
}
using std::tr1::is_same;
namespace __gnu_pbds
{
  struct null_mapped_type;
  struct rb_tree_tag;
  namespace detail
  {
    template < typename, typename, typename > struct basic_tree_policy_base;
    template
      <
      typename
      Const_Node_Iterator,
      typename
      Allocator
      >
      struct
      basic_tree_policy_base
      <Const_Node_Iterator, Const_Node_Iterator, Allocator >
    {};
  }
  template
    < typename, typename, typename, typename > struct null_tree_node_update;
template < typename Const_Node_Iterator, typename Node_Iterator, typename, typename Allocator > class tree_order_statistics_node_update:
  detail::basic_tree_policy_base
    < Const_Node_Iterator, Node_Iterator, Allocator >
  {
  public:
    typedef Allocator allocator_type;
    typedef typename allocator_type::size_type size_type;
    typedef size_type metadata_type;
    typedef Const_Node_Iterator const_node_iterator;
    typedef Node_Iterator node_iterator;
    typedef
      typename
      allocator_type::template
      rebind < metadata_type >::other::reference metadata_reference;
    void operator () (node_iterator, const_node_iterator) const;
  };
  template
    <
    typename
    Const_Node_Iterator,
    class
    Node_Iterator,
    class
    Cmp_Fn,
    class
    Allocator
    >
    inline
    void
    tree_order_statistics_node_update
    <
    Const_Node_Iterator,
    Node_Iterator,
    Cmp_Fn,
    Allocator
    >::operator
    () (node_iterator node_it, const_node_iterator end_nd_it) const
  {
    node_iterator l_child_it;
    size_type
      l_rank = (l_child_it == end_nd_it) ? : l_child_it.get_metadata ();
    node_iterator r_child_it = node_it.get_r_child ();
    size_type
      r_rank = (r_child_it == end_nd_it) ? : r_child_it.get_metadata ();
    const_cast
      < metadata_reference > (node_it.get_metadata ()) = l_rank + r_rank;
  }
  namespace
  {
    template < typename, typename, typename, bool > struct value_type_base;
    template
      <
      typename
      Key,
      typename
      Allocator
      > struct value_type_base <Key, null_mapped_type, Allocator, false >
    {
      typedef Key value_type;
      typedef
	typename
	Allocator::template rebind < value_type >::other value_type_allocator;
      typedef typename value_type_allocator::pointer pointer;
      typedef typename value_type_allocator::const_pointer const_pointer;
      typedef typename value_type_allocator::reference reference;
      typedef typename value_type_allocator::const_reference const_reference;
    };
    template
      <
      typename
      Key,
      typename
      Mapped, typename Alloc, bool Store_Extra > struct vt_base_selector
    {
      typedef value_type_base < Key, Mapped, Alloc, Store_Extra > type;
    };
    template
      <
      typename
      Key,
      typename
      Mapped,
      typename
      Alloc,
      bool
      Store_Extra
      >
      struct
      types_traits:vt_base_selector < Key, Mapped, Alloc, Store_Extra >::type
    {};
    template < typename, class, class > struct dumconst_node_iterator;
    template
      <
      typename
      Key,
      typename
      Mapped,
      class,
      class
      Node_And_It_Traits, class Allocator > class bin_search_tree_no_data_
    {
    protected:
      typedef
	typename
	Allocator::template
	rebind
	< typename Node_And_It_Traits::node >::other::pointer node_pointer;
      typedef
	typename
	types_traits
	< Key, Mapped, Allocator, false >::const_reference const_reference;
      typedef typename Node_And_It_Traits::point_iterator point_iterator;
      typedef typename Node_And_It_Traits::node_update node_update;
      void rotate_right (node_pointer);
      template
	<
	typename
	Node_Update_ > void apply_update (node_pointer, Node_Update_ *);
    };
    template
      <
      typename
      Key,
      typename
      Mapped,
      class
      Cmp_Fn,
      class
      Node_And_It_Traits,
      class
      Allocator
      >
      void
      bin_search_tree_no_data_
      <
      Key,
      Mapped,
      Cmp_Fn, Node_And_It_Traits, Allocator >::rotate_right (node_pointer p_x)
    {
      node_pointer p_y = p_x->m_p_parent;
      p_y->m_p_right = p_x;
      apply_update (p_x, this);
      apply_update (p_x->m_p_parent, (node_update *) this);
    }
    template
      <
      typename
      Key,
      typename
      Mapped,
      class
      Cmp_Fn,
      class
      Node_And_It_Traits,
      class
      Allocator
      >
      template
      <
      typename
      Node_Update_
      >
      void
      bin_search_tree_no_data_
      <
      Key,
      Mapped,
      Cmp_Fn,
      Node_And_It_Traits,
      Allocator >::apply_update (node_pointer p_nd, Node_Update_ *)
    {
      node_update ()((p_nd), ((0)));
    }
  }
  namespace detail
  {
  template < typename Key, typename Mapped, typename Cmp_Fn, typename Node_And_It_Traits, typename Allocator > class rb_tree_no_data_:
    bin_search_tree_no_data_
      < Key, Mapped, Cmp_Fn, Node_And_It_Traits, Allocator >
    {
      typedef
	bin_search_tree_no_data_
	< Key, Mapped, Cmp_Fn, Node_And_It_Traits, Allocator > base_type;
      typedef typename base_type::node_pointer node_pointer;
    public:
      typedef typename base_type::const_reference const_reference;
      typedef typename base_type::point_iterator point_iterator;
      std::pair < point_iterator, bool > insert (const_reference);
      void insert_fixup (node_pointer);
    };
    template
      <
      typename
      Key,
      typename
      Mapped,
      typename
      Cmp_Fn,
      typename
      Node_And_It_Traits,
      typename
      Allocator
      >
      std::pair
      <
      typename
      rb_tree_no_data_
      <
      Key,
      Mapped,
      Cmp_Fn,
      Node_And_It_Traits,
      Allocator
      >::point_iterator,
      bool
      >
      rb_tree_no_data_
      <
      Key,
      Mapped,
      Cmp_Fn, Node_And_It_Traits, Allocator >::insert (const_reference)
    {
      std::pair < point_iterator, bool > ins_pair;
{
	insert_fixup (ins_pair.first.m_p_nd);
      }
    }
    template
      <
      typename
      Key,
      typename
      Mapped,
      typename
      Cmp_Fn,
      typename
      Node_And_It_Traits,
      typename
      Allocator
      >
      void
      rb_tree_no_data_
      <
      Key,
      Mapped,
      Cmp_Fn,
      Node_And_It_Traits, Allocator >::insert_fixup (node_pointer p_nd)
    {
{
{
{
	    this->rotate_right (p_nd);
	  }
	}
      }
    }
    template
      <
      typename,
      typename, typename, typename, typename > struct container_base_dispatch;
    template
      <
      typename
      Key,
      typename
      Policy_Tl,
      typename
      Alloc
      >
      struct
      container_base_dispatch
      <Key, null_mapped_type, rb_tree_tag, Policy_Tl, Alloc >
    {
      typedef __gnu_cxx::typelist::at_index < Policy_Tl, 0 > at0;
      typedef typename at0::type at0t;
      typedef __gnu_cxx::typelist::at_index < Policy_Tl, 1 > at1;
      typedef typename at1::type at1t;
      typedef
	rb_tree_no_data_ < Key, null_mapped_type, at0t, at1t, Alloc > type;
    };
    template
      <
      typename
      Node_Pointer,
      typename,
      typename,
      typename,
      typename, typename, bool, class > class bin_search_tree_const_it_
    {
    public:
      Node_Pointer m_p_nd;
    };
    template
      <
      typename
      Node,
      class
      Const_Iterator,
      class Iterator, class Allocator > class bin_search_tree_const_node_it_
    {
      typedef
	typename
	Allocator::template rebind < Node >::other::pointer node_pointer;
    public:
      typedef typename Node::metadata_type metadata_type;
      typedef
	typename
	Allocator::template
	rebind
	< metadata_type >::other::const_reference const_metadata_reference;
    bin_search_tree_const_node_it_ (node_pointer p_nd):
      m_p_nd ((p_nd))
      {}
      const_metadata_reference get_metadata ()
      {
	return (m_p_nd->get_metadata ());
      }
      bin_search_tree_const_node_it_ ()
      {}
      bin_search_tree_const_node_it_
	< Node, Const_Iterator, Iterator, Allocator > get_r_child ()
      {
	return ((m_p_nd->m_p_right));
      }
      bool operator == (bin_search_tree_const_node_it_)
      {}
      node_pointer m_p_nd;
    };
    template
      <
      typename,
      typename,
      class,
      template
      <
      typename,
      class,
      class, class > class, class, class > struct bin_search_tree_traits;
    template
      <
      typename
      Key,
      class
      Cmp_Fn,
      template
      <
      typename,
      class,
      class,
      class
      >
      class
      Node_Update,
      class
      Node,
      class
      Allocator
      >
      struct
      bin_search_tree_traits
      <Key, null_mapped_type, Cmp_Fn, Node_Update, Node, Allocator >
    {
      typedef
	types_traits < Key, null_mapped_type, Allocator, false > type_traits;
      typedef Node node;
      typedef
	bin_search_tree_const_it_
	<
	typename
	Allocator::template
	rebind
	<
	node
	>::other::pointer,
	typename
	type_traits::value_type,
	typename
	type_traits::pointer,
	typename
	type_traits::const_pointer,
	typename
	type_traits::reference,
	typename
	type_traits::const_reference, true, Allocator > const_point_iterator;
      typedef const_point_iterator point_iterator;
      typedef
	bin_search_tree_const_node_it_
	<
	Node,
	const_point_iterator, point_iterator, Allocator > const_node_iterator;
      typedef const_node_iterator node_iterator;
      typedef
	Node_Update
	< const_node_iterator, node_iterator, Cmp_Fn, Allocator > node_update;
    };
    template < typename Node_Update, bool > struct tree_metadata_helper
    {
      typedef typename Node_Update::metadata_type type;
    };
    template
      <
      typename
      Key,
      typename
      Data,
      class
      Cmp_Fn,
      template
      <
      typename,
      class,
      class,
      class
      >
      class Node_Update, class Allocator > struct tree_node_metadata_selector
    {
      typedef
	dumconst_node_iterator < Key, Data, Allocator > dumconst_node_it;
      enum
      {
	null_update = is_same < Node_Update < dumconst_node_it,
	dumconst_node_it,
	Cmp_Fn,
	Allocator >,
	null_tree_node_update < dumconst_node_it,
	dumconst_node_it,
	Cmp_Fn,
	Allocator > >::value
      };
      typedef
	typename
	tree_metadata_helper
	<
	Node_Update
	<
	dumconst_node_it,
	dumconst_node_it, Cmp_Fn, Allocator >, null_update >::type type;
    };
    template
      <
      typename,
      typename,
      class,
      template
      <
      typename,
      class, class, class > class, class, class > struct tree_traits;
    template < typename Value_Type, class Metadata, class Allocator > struct rb_tree_node_
    {
      typedef Metadata metadata_type;
      typedef
	typename
	Allocator::template
	rebind
	<
	rb_tree_node_
	< Value_Type, Metadata, Allocator > >::other::pointer node_pointer;
      typedef
	typename
	Allocator::template
	rebind < metadata_type >::other::reference metadata_reference;
        metadata_reference get_metadata ()
      {
	return m_metadata;
      }
      node_pointer m_p_right;
      node_pointer m_p_parent;
      metadata_type m_metadata;
    };
    template
      <
      typename
      Key,
      typename
      Mapped,
      typename
      Cmp_Fn,
      template
      <
      typename,
      class,
      class,
      class
      >
      class
      Node_Update,
      typename
      Allocator
      >
      struct
      tree_traits
      <Key,
      Mapped,
      Cmp_Fn,
      Node_Update,
      rb_tree_tag,
      Allocator
      >:bin_search_tree_traits
      <
      Key,
      Mapped,
      Cmp_Fn,
      Node_Update,
      rb_tree_node_
      <
      typename
      types_traits
      <
      Key,
      Mapped,
      Allocator,
      false
      >::value_type,
      typename
      tree_node_metadata_selector
      <
      Key,
      Mapped, Cmp_Fn, Node_Update, Allocator >::type, Allocator >, Allocator >
    {};
  }
template < typename Key, typename Mapped, typename Tag, typename Policy_Tl, typename Allocator > class container_base:
  public
    detail::container_base_dispatch
    < Key, Mapped, Tag, Policy_Tl, Allocator >::type
  {};
template < typename Key, typename Mapped, typename Tag, typename, typename Policy_Tl, typename Allocator > class basic_tree:
  public
    container_base < Key, Mapped, Tag, Policy_Tl, Allocator >
  {};
  template
    <
    typename
    Key,
    typename
    Mapped,
    typename
    Cmp_Fn
    =
    std::less
    <
    Key
    >,
    typename
    Tag
    =
    rb_tree_tag,
    template
    <
    typename,
    typename,
    typename,
    typename
    >
    class
    Node_Update
    =
    null_tree_node_update,
    typename
    Allocator
    =
    std::allocator
    <
    char
    > >class
    tree:public
    basic_tree
    <
    Key,
    Mapped,
    Tag,
    detail::tree_traits
    <
    Key,
    Mapped,
    Cmp_Fn,
    Node_Update,
    Tag,
    Allocator
    >,
    typename
    __gnu_cxx::typelist::create2
    <
    Cmp_Fn,
    detail::tree_traits
    < Key, Mapped, Cmp_Fn, Node_Update, Tag, Allocator > >::type, Allocator >
  {};
}
using namespace std;
using namespace __gnu_pbds;
typedef
  tree
  <
  int,
  null_mapped_type,
  less < int >, rb_tree_tag, tree_order_statistics_node_update > set_t;
main ()
{
  set_t s;
  s.insert (12);
}
