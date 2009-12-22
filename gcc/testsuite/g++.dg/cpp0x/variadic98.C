// PR c++/42358
// { dg-do assemble }
// { dg-options -std=c++0x }

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;
namespace std __attribute__ ((__visibility__ ("default"))) {
    using ::size_t;
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    struct __sfinae_types   {
	typedef char __one;
	typedef struct {
	} __two;
    };
    template<typename _Tp, _Tp __v>     struct integral_constant     {
	static const _Tp value = __v;
	typedef _Tp value_type;
	typedef integral_constant<_Tp, __v> type;
    };
    typedef integral_constant<bool, false> false_type;
    template<typename>     struct remove_cv;
    template<typename>     struct __is_void_helper     : public false_type {
    };
    template<typename _Tp>     struct is_void     : public integral_constant<bool, (__is_void_helper<typename           remove_cv<_Tp>::type>::value)>     {
    };
    template<typename>     struct is_array     : public false_type {
    };
    template<typename>     struct is_function     : public false_type {
    };
    template<typename, unsigned _Uint = 0>     struct extent     : public integral_constant<std::size_t, 0> {
    };
    template<typename _Tp>     struct remove_const     {
	typedef _Tp type;
    };
    template<typename _Tp>     struct remove_volatile     {
	typedef _Tp type;
    };
    template<typename _Tp>     struct remove_cv     {
	typedef typename       remove_const<typename remove_volatile<_Tp>::type>::type type;
    };
    template<typename>     struct is_lvalue_reference     : public false_type {
    };
    template<typename>     struct is_rvalue_reference     : public false_type {
    };
    template<typename _Tp>     struct is_reference     : public integral_constant<bool, (is_lvalue_reference<_Tp>::value           || is_rvalue_reference<_Tp>::value)>     {
    };
    template<typename _Tp>     struct remove_reference     {
	typedef _Tp type;
    };
    template<typename _Tp,     bool = !is_reference<_Tp>::value && !is_void<_Tp>::value>     struct __add_rvalue_reference_helper     {
	typedef _Tp type;
    };
    template<typename _Tp>     struct add_rvalue_reference     : public __add_rvalue_reference_helper<_Tp>     {
    };
    template<typename _Tp>     typename add_rvalue_reference<_Tp>::type declval();
    template<typename _From, typename _To,     bool = (is_void<_From>::value || is_void<_To>::value      || is_function<_To>::value || is_array<_To>::value)>     struct __is_convertible_helper     {
    };
    template<typename _From, typename _To>     struct __is_convertible_helper<_From, _To, false>     : public __sfinae_types     {
	static __one __test(_To);
	static __two __test(...);
	static const bool __value = sizeof(__test(declval<_From>())) == 1;
    };
    template<typename _From, typename _To>     struct is_convertible     : public integral_constant<bool,           __is_convertible_helper<_From, _To>::__value>     {
    };
    template<bool, typename _Tp = void>     struct enable_if     {
    };
    template<typename _Tp>     struct enable_if<true, _Tp>     {
	typedef _Tp type;
    };
    template<typename _Tp>     struct identity     {
	typedef _Tp type;
    };
    template<typename _Tp>     inline typename enable_if<!is_lvalue_reference<_Tp>::value, _Tp&&>::type     forward(typename std::identity<_Tp>::type& __t)     {
    }
    template<typename _Tp>     inline typename enable_if<is_lvalue_reference<_Tp>::value, _Tp>::type     forward(typename std::identity<_Tp>::type __t)     {
    }
    template<typename _Tp>     inline typename std::remove_reference<_Tp>::type&&     move(_Tp&& __t)     {
    }
    template<class _T1, class _T2>     struct pair     {
	typedef _T1 first_type;
	typedef _T2 second_type;
	_T1 first;
	_T2 second;
	template<class _U1, class = typename         std::enable_if<std::is_convertible<_U1, _T1>::value>::type>         pair(_U1&& __x, const _T2& __y)  : first(std::forward<_U1>(__x)),    second(__y) {
	}
	template<class _U2, class = typename         std::enable_if<std::is_convertible<_U2, _T2>::value>::type>         pair(const _T1& __x, _U2&& __y)  : first(__x),    second(std::forward<_U2>(__y)) {
	}
	template<class _U1, class _U2, class = typename         std::enable_if<std::is_convertible<_U1, _T1>::value          && std::is_convertible<_U2, _T2>::value>::type>         pair(_U1&& __x, _U2&& __y)  : first(std::forward<_U1>(__x)),    second(std::forward<_U2>(__y)) {
	}
	template<class _U1, class _U2>         pair(pair<_U1, _U2>&& __p)  : first(std::move(__p.first)),    second(std::move(__p.second)) {
	}
	template<class _U1, class _U2>         pair&         operator=(pair<_U1, _U2>&& __p)  {
	}
    };
    struct input_iterator_tag {
    };
    struct output_iterator_tag {
    };
    struct forward_iterator_tag : public input_iterator_tag {
    };
    struct bidirectional_iterator_tag : public forward_iterator_tag {
    };
    template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t,            typename _Pointer = _Tp*, typename _Reference = _Tp&>     struct iterator     {
	typedef _Category iterator_category;
	typedef _Tp value_type;
	typedef _Distance difference_type;
	typedef _Pointer pointer;
	typedef _Reference reference;
    };
    template<typename _Iterator>     struct iterator_traits     {
	typedef typename _Iterator::iterator_category iterator_category;
	typedef typename _Iterator::value_type value_type;
	typedef typename _Iterator::difference_type difference_type;
	typedef typename _Iterator::pointer pointer;
	typedef typename _Iterator::reference reference;
    };
    template<typename _Iter>     inline typename iterator_traits<_Iter>::iterator_category     __iterator_category(const _Iter&)     {
    }
    template<typename _InputIterator>     inline typename iterator_traits<_InputIterator>::difference_type     __distance(_InputIterator __first, _InputIterator __last,                input_iterator_tag)     {
    }
    template<typename _InputIterator>     inline typename iterator_traits<_InputIterator>::difference_type     distance(_InputIterator __first, _InputIterator __last)     {
	return std::__distance(__first, __last,         std::__iterator_category(__first));
    }
    template<typename _Iterator>     class reverse_iterator     : public iterator<typename iterator_traits<_Iterator>::iterator_category,         typename iterator_traits<_Iterator>::value_type,         typename iterator_traits<_Iterator>::difference_type,         typename iterator_traits<_Iterator>::pointer,                       typename iterator_traits<_Iterator>::reference>     {
    };
    template<typename _Container>     class back_insert_iterator     : public iterator<output_iterator_tag, void, void, void, void>     {
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     {
    public:
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef _Tp* pointer;
	typedef const _Tp* const_pointer;
	typedef _Tp& reference;
	typedef const _Tp& const_reference;
	typedef _Tp value_type;
	new_allocator() throw() {
	}
	new_allocator(const new_allocator&) throw() {
	}
	template<typename _Tp1>         new_allocator(const new_allocator<_Tp1>&) throw() {
	}
	template<typename... _Args>         void         construct(pointer __p, _Args&&... __args)  {
	}
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    public:
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef _Tp* pointer;
	typedef const _Tp* const_pointer;
	typedef _Tp& reference;
	typedef const _Tp& const_reference;
	typedef _Tp value_type;
	template<typename _Tp1>         struct rebind         {
	    typedef allocator<_Tp1> other;
	};
	allocator() throw() {
	}
	template<typename _Tp1>         allocator(const allocator<_Tp1>&) throw() {
	}
    };
    extern template class allocator<char>;
    extern template class allocator<wchar_t>;
    template<typename _Arg, typename _Result>     struct unary_function     {
	typedef _Arg argument_type;
	typedef _Result result_type;
    };
    template<typename _Arg1, typename _Arg2, typename _Result>     struct binary_function     {
	typedef _Arg1 first_argument_type;
	typedef _Arg2 second_argument_type;
	typedef _Result result_type;
    };
    template<typename _Tp>     struct less : public binary_function<_Tp, _Tp, bool>     {
	bool       operator()(const _Tp& __x, const _Tp& __y) const       {
	}
    };
    template<typename _Pair>     struct _Select1st : public unary_function<_Pair,            typename _Pair::first_type>     {
	const typename _Pair::first_type&       operator()(const _Pair& __x) const       {
	}
    };
    struct _Rb_tree_node_base   {
	typedef _Rb_tree_node_base* _Base_ptr;
	typedef const _Rb_tree_node_base* _Const_Base_ptr;
    };
    template<typename _Val>     struct _Rb_tree_node : public _Rb_tree_node_base     {
	typedef _Rb_tree_node<_Val>* _Link_type;
	_Val _M_value_field;
	template<typename... _Args>         _Rb_tree_node(_Args&&... __args)  : _Rb_tree_node_base(),    _M_value_field(std::forward<_Args>(__args)...) {
	}
    };
    template<typename _Tp>     struct _Rb_tree_iterator     {
	typedef _Tp value_type;
	typedef _Tp& reference;
	typedef _Tp* pointer;
	typedef bidirectional_iterator_tag iterator_category;
	typedef ptrdiff_t difference_type;
	typedef _Rb_tree_iterator<_Tp> _Self;
	typedef _Rb_tree_node_base::_Base_ptr _Base_ptr;
	typedef _Rb_tree_node<_Tp>* _Link_type;
	_Base_ptr _M_node;
    };
    template<typename _Tp>     struct _Rb_tree_const_iterator     {
	typedef _Tp value_type;
	typedef const _Tp& reference;
	typedef const _Tp* pointer;
	typedef _Rb_tree_iterator<_Tp> iterator;
	typedef bidirectional_iterator_tag iterator_category;
	typedef ptrdiff_t difference_type;
	typedef _Rb_tree_const_iterator<_Tp> _Self;
	typedef _Rb_tree_node_base::_Const_Base_ptr _Base_ptr;
	typedef const _Rb_tree_node<_Tp>* _Link_type;
	explicit       _Rb_tree_const_iterator(_Link_type __x)       : _M_node(__x) {
	}
	_Rb_tree_const_iterator(const iterator& __it)       : _M_node(__it._M_node) {
	}
	_Base_ptr _M_node;
    };
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc = allocator<_Val> >     class _Rb_tree     {
	typedef typename _Alloc::template rebind<_Rb_tree_node<_Val> >::other               _Node_allocator;
	typedef _Rb_tree_node_base* _Base_ptr;
	typedef const _Rb_tree_node_base* _Const_Base_ptr;
    public:
	typedef _Key key_type;
	typedef _Val value_type;
	typedef value_type* pointer;
	typedef const value_type* const_pointer;
	typedef value_type& reference;
	typedef const value_type& const_reference;
	typedef _Rb_tree_node<_Val>* _Link_type;
	typedef const _Rb_tree_node<_Val>* _Const_Link_type;
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef _Alloc allocator_type;
	_Node_allocator&       _M_get_Node_allocator()       {
	}
	_Link_type       _M_get_node()       {
	}
	template<typename... _Args>         _Link_type         _M_create_node(_Args&&... __args)  {
	    _Link_type __tmp = _M_get_node();
	    try      {
		_M_get_Node_allocator().construct(__tmp,           std::forward<_Args>(__args)...);
	    }
	    catch(...)      {
	    }
	}
	template<typename _Key_compare,         bool _Is_pod_comparator = __is_pod(_Key_compare)>         struct _Rb_tree_impl : public _Node_allocator         {
	    _Key_compare _M_key_compare;
	    _Rb_tree_node_base _M_header;
	    size_type _M_node_count;
	    _Rb_tree_impl(const _Key_compare& __comp, const _Node_allocator& __a)    : _Node_allocator(__a), _M_key_compare(__comp), _M_header(),      _M_node_count(0)    {
	    }
	    void    _M_initialize()    {
	    }
	};
	_Rb_tree_impl<_Compare> _M_impl;
	_Base_ptr&       _M_rightmost()       {
	}
	_Link_type       _M_begin()       {
	}
	_Link_type       _M_end()       {
	}
	_Const_Link_type       _M_end() const       {
	}
	static _Link_type       _S_right(_Base_ptr __x)       {
	}
	static const_reference       _S_value(_Const_Base_ptr __x)       {
	}
	static const _Key&       _S_key(_Const_Base_ptr __x)       {
	    return _KeyOfValue()(_S_value(__x));
	}
	typedef _Rb_tree_iterator<value_type> iterator;
	typedef _Rb_tree_const_iterator<value_type> const_iterator;
	typedef std::reverse_iterator<iterator> reverse_iterator;
	typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
	iterator       _M_insert_(_Const_Base_ptr __x, _Const_Base_ptr __y,    const value_type& __v);
	iterator       _M_insert_lower(_Base_ptr __x, _Base_ptr __y, const value_type& __v);
	iterator       _M_insert_equal_lower(const value_type& __x);
	iterator       _M_lower_bound(_Link_type __x, _Link_type __y,        const _Key& __k);
	iterator       _M_upper_bound(_Link_type __x, _Link_type __y,        const _Key& __k);
	_Rb_tree(const _Compare& __comp,         const allocator_type& __a = allocator_type())       : _M_impl(__comp, __a) {
	}
	iterator       end()       {
	}
	iterator       _M_insert_equal_(const_iterator __position, const value_type& __x);
	template<typename _InputIterator>         void         _M_insert_unique(_InputIterator __first, _InputIterator __last);
	template<typename _InputIterator>         void         _M_insert_equal(_InputIterator __first, _InputIterator __last);
	size_type       count(const key_type& __k) const;
	pair<iterator, iterator>       equal_range(const key_type& __k);
	pair<const_iterator, const_iterator>       equal_range(const key_type& __k) const;
    };
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     _M_insert_(_Const_Base_ptr __x, _Const_Base_ptr __p, const _Val& __v)     {
	_Link_type __z = _M_create_node(__v);
    }
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     _M_insert_lower(_Base_ptr __x, _Base_ptr __p, const _Val& __v)     {
	_Link_type __z = _M_create_node(__v);
    }
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     _M_insert_equal_lower(const _Val& __v)     {
	_Link_type __x = _M_begin();
	_Link_type __y = _M_end();
	return _M_insert_lower(__x, __y, __v);
    }
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,       _Compare, _Alloc>::iterator,   typename _Rb_tree<_Key, _Val, _KeyOfValue,       _Compare, _Alloc>::iterator>     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     equal_range(const _Key& __k)     {
	_Link_type __x = _M_begin();
	_Link_type __y = _M_end();
	while (__x != 0)  {
	    if (_M_impl._M_key_compare(_S_key(__x), __k))      __x = _S_right(__x);
	    else      {
		_Link_type __xu(__x), __yu(__y);
		return pair<iterator,             iterator>(_M_lower_bound(__x, __y, __k),         _M_upper_bound(__xu, __yu, __k));
	    }
	}
    }
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     pair<typename _Rb_tree<_Key, _Val, _KeyOfValue,       _Compare, _Alloc>::const_iterator,   typename _Rb_tree<_Key, _Val, _KeyOfValue,       _Compare, _Alloc>::const_iterator>     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     equal_range(const _Key& __k) const     {
	_Const_Link_type __y = _M_end();
	return pair<const_iterator, const_iterator>(const_iterator(__y),         const_iterator(__y));
    }
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::iterator     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     _M_insert_equal_(const_iterator __position, const _Val& __v)     {
	if (__position._M_node == _M_end())  {
	    if (__position._M_node == _M_rightmost())      return _M_insert_(0, _M_rightmost(), __v);
	    else      return _M_insert_equal_lower(__v);
	}
    }
    template<typename _Key, typename _Val, typename _KoV,            typename _Cmp, typename _Alloc>     template<class _II>       void       _Rb_tree<_Key, _Val, _KoV, _Cmp, _Alloc>::       _M_insert_equal(_II __first, _II __last)       {
	for (;
	     __first != __last;
	     ++__first)    _M_insert_equal_(end(), *__first);
    }
    template<typename _Key, typename _Val, typename _KeyOfValue,            typename _Compare, typename _Alloc>     typename _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::size_type     _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::     count(const _Key& __k) const     {
	pair<const_iterator, const_iterator> __p = equal_range(__k);
	const size_type __n = std::distance(__p.first, __p.second);
    }
    template<class _E>     class initializer_list     {
    public:
	typedef _E value_type;
	typedef const _E& reference;
	typedef const _E& const_reference;
	typedef size_t size_type;
	typedef const _E* iterator;
	typedef const _E* const_iterator;
	iterator _M_array;
	size_type _M_len;
	initializer_list(const_iterator __a, size_type __l)       : _M_array(__a), _M_len(__l) {
	}
	const_iterator       begin() const {
	}
	const_iterator       end() const {
	}
    };
    template <typename _Key, typename _Tp,      typename _Compare = std::less<_Key>,      typename _Alloc = std::allocator<std::pair<const _Key, _Tp> > >     class multimap     {
	typedef _Key key_type;
	typedef _Tp mapped_type;
	typedef std::pair<const _Key, _Tp> value_type;
	typedef _Compare key_compare;
	typedef _Alloc allocator_type;
	typedef typename _Alloc::value_type _Alloc_value_type;
	typedef typename _Alloc::template rebind<value_type>::other         _Pair_alloc_type;
	typedef _Rb_tree<key_type, value_type, _Select1st<value_type>,          key_compare, _Pair_alloc_type> _Rep_type;
	_Rep_type _M_t;
    public:
	typedef typename _Pair_alloc_type::pointer pointer;
	typedef typename _Pair_alloc_type::const_pointer const_pointer;
	typedef typename _Pair_alloc_type::reference reference;
	typedef typename _Pair_alloc_type::const_reference const_reference;
	typedef typename _Rep_type::iterator iterator;
	typedef typename _Rep_type::const_iterator const_iterator;
	typedef typename _Rep_type::size_type size_type;
	typedef typename _Rep_type::difference_type difference_type;
	typedef typename _Rep_type::reverse_iterator reverse_iterator;
	typedef typename _Rep_type::const_reverse_iterator const_reverse_iterator;
	multimap(initializer_list<value_type> __l,         const _Compare& __comp = _Compare(),         const allocator_type& __a = allocator_type())       : _M_t(__comp, __a)       {
	    _M_t._M_insert_equal(__l.begin(), __l.end());
	}
	template<typename _InputIterator>         multimap(_InputIterator __first, _InputIterator __last)  : _M_t()         {
	}
	template<typename _InputIterator>         multimap(_InputIterator __first, _InputIterator __last,    const _Compare& __comp,    const allocator_type& __a = allocator_type())         : _M_t(__comp, __a)         {
	}
	template<typename _InputIterator>         void         insert(_InputIterator __first, _InputIterator __last)         {
	}
	size_type       count(const key_type& __x) const       {
	    return _M_t.count(__x);
	}
	std::pair<iterator, iterator>       equal_range(const key_type& __x)       {
	    return _M_t.equal_range(__x);
	}
	template<typename _K1, typename _T1, typename _C1, typename _A1>         friend bool         operator==(const multimap<_K1, _T1, _C1, _A1>&,      const multimap<_K1, _T1, _C1, _A1>&);
	template<typename _K1, typename _T1, typename _C1, typename _A1>         friend bool         operator<(const multimap<_K1, _T1, _C1, _A1>&,     const multimap<_K1, _T1, _C1, _A1>&);
    };
}
extern "C" {
    extern void __assert_fail (__const char *__assertion, __const char *__file,       unsigned int __line, __const char *__function)      throw () __attribute__ ((__noreturn__));
}
using namespace std;
int test01() {
    typedef multimap<int,double> Container;
    typedef Container::iterator iterator;
    typedef pair<iterator,iterator> itpair;
    Container m({
		{
		1, 1.0 }
		}
	       );
    itpair ip = m.equal_range(1);
    ((distance(ip.first, ip.second) == 3) ? static_cast<void> (0) : __assert_fail ("distance(ip.first, ip.second) == 3", "/home/richard/src/trunk/libstdc++-v3/testsuite/23_containers/multimap/init-list.cc", 36, __PRETTY_FUNCTION__));
    ((m.count(7) == 2) ? static_cast<void> (0) : __assert_fail ("m.count(7) == 2", "/home/richard/src/trunk/libstdc++-v3/testsuite/23_containers/multimap/init-list.cc", 54, __PRETTY_FUNCTION__));
}
