// { dg-do compile }
// { dg-options "-fgnu-tm" }

typedef long int ptrdiff_t;
typedef long unsigned int size_t;
namespace std __attribute__ ((__visibility__ ("default")))
{
  using::ptrdiff_t;
  using::size_t;
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  struct input_iterator_tag
  {
  };
  struct output_iterator_tag
  {
  };
  struct forward_iterator_tag:public input_iterator_tag
  {
  };
  struct bidirectional_iterator_tag:public forward_iterator_tag
  {
  };
  struct random_access_iterator_tag:public bidirectional_iterator_tag
  {
  };
  template < typename _Category, typename _Tp, typename _Distance =
    ptrdiff_t, typename _Pointer = _Tp *, typename _Reference =
    _Tp & >struct iterator
  {
    typedef _Category iterator_category;
    typedef _Tp value_type;
    typedef _Distance difference_type;
    typedef _Pointer pointer;
    typedef _Reference reference;
  };
  template < typename _Iterator > struct iterator_traits
  {
    typedef typename _Iterator::iterator_category iterator_category;
    typedef typename _Iterator::value_type value_type;
    typedef typename _Iterator::difference_type difference_type;
    typedef typename _Iterator::pointer pointer;
    typedef typename _Iterator::reference reference;
  };
  template < typename _Tp > struct iterator_traits <_Tp * >
  {
    typedef random_access_iterator_tag iterator_category;
    typedef _Tp value_type;
    typedef ptrdiff_t difference_type;
    typedef _Tp *pointer;
    typedef _Tp & reference;
  };
  template < typename _Tp > struct iterator_traits <const _Tp *>
  {
    typedef random_access_iterator_tag iterator_category;
    typedef _Tp value_type;
    typedef ptrdiff_t difference_type;
    typedef const _Tp *pointer;
    typedef const _Tp & reference;
  };
  template < typename _Iter > inline typename iterator_traits <
    _Iter >::iterator_category __iterator_category (const _Iter &)
  {
    return typename iterator_traits < _Iter >::iterator_category ();
  }
}

namespace std __attribute__ ((__visibility__ ("default")))
{
template < typename _Iterator > class reverse_iterator:public iterator < typename iterator_traits < _Iterator >::iterator_category,
    typename iterator_traits < _Iterator >::value_type,
    typename iterator_traits < _Iterator >::difference_type,
    typename iterator_traits < _Iterator >::pointer,
    typename iterator_traits < _Iterator >::reference >
  {
  protected:_Iterator current;
    typedef iterator_traits < _Iterator > __traits_type;
  public:typedef _Iterator iterator_type;
    typedef typename __traits_type::difference_type difference_type;
    typedef typename __traits_type::pointer pointer;
    typedef typename __traits_type::reference reference;
  reverse_iterator ():current ()
    {
    } explicit reverse_iterator (iterator_type __x):current (__x)
    {
    } reverse_iterator (const reverse_iterator & __x):current (__x.current)
    {
    } template < typename _Iter > reverse_iterator (const reverse_iterator <
						    _Iter >
						    &__x):current (__x.
								   base ())
    {
    } iterator_type base () const
    {
      return current;
    }
    reference operator* () const
    {
      _Iterator __tmp = current;
	return *--__tmp;
    }
    pointer operator-> () const
    {
      return &(operator* ());
    }
    reverse_iterator & operator++ ()
    {
      --current;
      return *this;
    }
    reverse_iterator operator++ (int)
    {
      reverse_iterator __tmp = *this;
      --current;
      return __tmp;
    }
    reverse_iterator & operator-- ()
    {
      ++current;
      return *this;
    }
    reverse_iterator operator-- (int)
    {
      reverse_iterator __tmp = *this;
      ++current;
      return __tmp;
    }
    reverse_iterator operator+ (difference_type __n) const
    {
      return reverse_iterator (current - __n);
    }
    reverse_iterator & operator+= (difference_type __n)
    {
      current -= __n;
      return *this;
    }
    reverse_iterator operator- (difference_type __n) const
    {
      return reverse_iterator (current + __n);
    }
    reverse_iterator & operator-= (difference_type __n)
    {
      current += __n;
      return *this;
    }
    reference operator[] (difference_type __n) const
    {
      return *(*this + __n);
    }
  };
  template < typename _Iterator >
    inline bool operator== (const reverse_iterator < _Iterator > &__x,
			    const reverse_iterator < _Iterator > &__y)
  {
    return __x.base () == __y.base ();
  }
  template < typename _Iterator >
    inline bool operator< (const reverse_iterator < _Iterator > &__x,
			   const reverse_iterator < _Iterator > &__y)
  {
    return __y.base () < __x.base ();
  }
  template < typename _Iterator >
    inline bool operator!= (const reverse_iterator < _Iterator > &__x,
			    const reverse_iterator < _Iterator > &__y)
  {
    return !(__x == __y);
  }
  template < typename _Iterator >
    inline bool operator> (const reverse_iterator < _Iterator > &__x,
			   const reverse_iterator < _Iterator > &__y)
  {
    return __y < __x;
  }
  template < typename _Iterator >
    inline bool operator<= (const reverse_iterator < _Iterator > &__x,
			    const reverse_iterator < _Iterator > &__y)
  {
    return !(__y < __x);
  }
  template < typename _Iterator >
    inline bool operator>= (const reverse_iterator < _Iterator > &__x,
			    const reverse_iterator < _Iterator > &__y)
  {
    return !(__x < __y);
  }
  template < typename _Iterator > inline typename reverse_iterator <
    _Iterator >::difference_type operator- (const reverse_iterator <
					    _Iterator > &__x,
					    const reverse_iterator <
					    _Iterator > &__y)
  {
    return __y.base () - __x.base ();
  }
  template < typename _Iterator > inline reverse_iterator < _Iterator >
    operator+ (typename reverse_iterator < _Iterator >::difference_type __n,
	       const reverse_iterator < _Iterator > &__x)
  {
    return reverse_iterator < _Iterator > (__x.base () - __n);
  }
  template < typename _IteratorL,
    typename _IteratorR > inline bool operator== (const reverse_iterator <
						  _IteratorL > &__x,
						  const reverse_iterator <
						  _IteratorR > &__y)
  {
    return __x.base () == __y.base ();
  }
  template < typename _IteratorL,
    typename _IteratorR > inline bool operator< (const reverse_iterator <
						 _IteratorL > &__x,
						 const reverse_iterator <
						 _IteratorR > &__y)
  {
    return __y.base () < __x.base ();
  }
  template < typename _IteratorL,
    typename _IteratorR > inline bool operator!= (const reverse_iterator <
						  _IteratorL > &__x,
						  const reverse_iterator <
						  _IteratorR > &__y)
  {
    return !(__x == __y);
  }
  template < typename _IteratorL,
    typename _IteratorR > inline bool operator> (const reverse_iterator <
						 _IteratorL > &__x,
						 const reverse_iterator <
						 _IteratorR > &__y)
  {
    return __y < __x;
  }
  template < typename _IteratorL,
    typename _IteratorR > inline bool operator<= (const reverse_iterator <
						  _IteratorL > &__x,
						  const reverse_iterator <
						  _IteratorR > &__y)
  {
    return !(__y < __x);
  }
  template < typename _IteratorL,
    typename _IteratorR > inline bool operator>= (const reverse_iterator <
						  _IteratorL > &__x,
						  const reverse_iterator <
						  _IteratorR > &__y)
  {
    return !(__x < __y);
  }
  template < typename _IteratorL,
    typename _IteratorR > inline typename reverse_iterator <
    _IteratorL >::difference_type operator- (const reverse_iterator <
					     _IteratorL > &__x,
					     const reverse_iterator <
					     _IteratorR > &__y)
  {
    return __y.base () - __x.base ();
  }
template < typename _Container > class back_insert_iterator:public iterator < output_iterator_tag, void, void, void,
    void >
  {
  protected:_Container * container;
  public:typedef _Container container_type;
    explicit back_insert_iterator (_Container & __x):container (&__x)
    {
    } back_insert_iterator & operator= (typename _Container::
					const_reference __value)
    {
      container->push_back (__value);
      return *this;
    }
    back_insert_iterator & operator* ()
    {
      return *this;
    }
    back_insert_iterator & operator++ ()
    {
      return *this;
    }
    back_insert_iterator operator++ (int)
    {
      return *this;
    }
  };
  template < typename _Container > inline back_insert_iterator < _Container >
    back_inserter (_Container & __x)
  {
    return back_insert_iterator < _Container > (__x);
  }
template < typename _Container > class front_insert_iterator:public iterator < output_iterator_tag, void, void, void,
    void >
  {
  protected:_Container * container;
  public:typedef _Container container_type;
    explicit front_insert_iterator (_Container & __x):container (&__x)
    {
    } front_insert_iterator & operator= (typename _Container::
					 const_reference __value)
    {
      container->push_front (__value);
      return *this;
    }
    front_insert_iterator & operator* ()
    {
      return *this;
    }
    front_insert_iterator & operator++ ()
    {
      return *this;
    }
    front_insert_iterator operator++ (int)
    {
      return *this;
    }
  };
  template < typename _Container > inline front_insert_iterator < _Container >
    front_inserter (_Container & __x)
  {
    return front_insert_iterator < _Container > (__x);
  }
template < typename _Container > class insert_iterator:public iterator < output_iterator_tag, void, void, void,
    void >
  {
  protected:_Container * container;
    typename _Container::iterator iter;
  public:typedef _Container container_type;
      insert_iterator (_Container & __x,
		       typename _Container::iterator __i):container (&__x),
      iter (__i)
    {
    } insert_iterator & operator= (typename _Container::
				   const_reference __value)
    {
      iter = container->insert (iter, __value);
      ++iter;
      return *this;
    }
    insert_iterator & operator* ()
    {
      return *this;
    }
    insert_iterator & operator++ ()
    {
      return *this;
    }
    insert_iterator & operator++ (int)
    {
      return *this;
    }
  };
  template < typename _Container,
    typename _Iterator > inline insert_iterator < _Container >
    inserter (_Container & __x, _Iterator __i)
  {
    return insert_iterator < _Container > (__x,
					   typename _Container::
					   iterator (__i));
  }
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  using std::size_t;
  using std::ptrdiff_t;
  template < typename _Tp > class new_allocator
  {
  public:typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef _Tp *pointer;
    typedef const _Tp *const_pointer;
    typedef _Tp & reference;
    typedef const _Tp & const_reference;
    typedef _Tp value_type;
    template < typename _Tp1 > struct rebind
    {
      typedef new_allocator < _Tp1 > other;
    };
    new_allocator ()throw ()
    {
    } new_allocator (const new_allocator &) throw ()
    {
    } template < typename _Tp1 > new_allocator (const new_allocator < _Tp1 >
						&) throw ()
    {
    } ~new_allocator ()throw ()
    {
    } pointer address (reference __x) const
    {
      return &__x;
    }
    const_pointer address (const_reference __x) const
    {
      return &__x;
    }
    pointer allocate (size_type __n, const void * = 0)
    {
      return static_cast < _Tp * >(::operator  new (__n * sizeof (_Tp)));
    }
    void deallocate (pointer __p, size_type)
    {
      ::operator  delete (__p);
    } size_type max_size () const throw ()
    {
      return size_t (-1) / sizeof (_Tp);
    }
    void construct (pointer __p, const _Tp & __val)
    {
      ::new ((void *) __p) _Tp (__val);
    } void destroy (pointer __p)
    {
      __p->~_Tp ();
  }};
  template < typename _Tp > inline bool operator== (const new_allocator <
						    _Tp > &,
						    const new_allocator <
						    _Tp > &)
  {
    return true;
  }
  template < typename _Tp > inline bool operator!= (const new_allocator <
						    _Tp > &,
						    const new_allocator <
						    _Tp > &)
  {
    return false;
  }
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp > class allocator;
  template <> class allocator < void >
  {
  public:typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef void *pointer;
    typedef const void *const_pointer;
    typedef void value_type;
      template < typename _Tp1 > struct rebind
    {
      typedef allocator < _Tp1 > other;
    };
  };
template < typename _Tp > class allocator:public __gnu_cxx::new_allocator <
    _Tp >
  {
  public:typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef _Tp *pointer;
    typedef const _Tp *const_pointer;
    typedef _Tp & reference;
    typedef const _Tp & const_reference;
    typedef _Tp value_type;
    template < typename _Tp1 > struct rebind
    {
      typedef allocator < _Tp1 > other;
    };
    allocator ()throw ()
    {
    } allocator (const allocator & __a) throw ():__gnu_cxx::new_allocator <
      _Tp > (__a)
    {
    } template < typename _Tp1 > allocator (const allocator < _Tp1 >
					    &) throw ()
    {
    } ~allocator ()throw ()
    {
  }};
  template < typename _T1,
    typename _T2 > inline bool operator== (const allocator < _T1 > &,
					   const allocator < _T2 > &)
  {
    return true;
  }
  template < typename _Tp > inline bool operator== (const allocator < _Tp > &,
						    const allocator < _Tp > &)
  {
    return true;
  }
  template < typename _T1,
    typename _T2 > inline bool operator!= (const allocator < _T1 > &,
					   const allocator < _T2 > &)
  {
    return false;
  }
  template < typename _Tp > inline bool operator!= (const allocator < _Tp > &,
						    const allocator < _Tp > &)
  {
    return false;
  }
  template < typename _Alloc, bool = __is_empty (_Alloc) > struct __alloc_swap
  {
    static void _S_do_it (_Alloc &, _Alloc &)
    {
  }};
  template < typename _Alloc > struct __alloc_swap <_Alloc, false >
  {
    static void _S_do_it (_Alloc & __one, _Alloc & __two)
    {
      if (__one != __two)
	swap (__one, __two);
    }
  };
  template < typename _Alloc, bool = __is_empty (_Alloc) > struct __alloc_neq
  {
    static bool _S_do_it (const _Alloc &, const _Alloc &)
    {
      return false;
    }
  };
  template < typename _Alloc > struct __alloc_neq <_Alloc, false >
  {
    static bool _S_do_it (const _Alloc & __one, const _Alloc & __two)
    {
      return __one != __two;
    }
  };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  struct _List_node_base
  {
    _List_node_base *_M_next;
    _List_node_base *_M_prev;
    static void swap (_List_node_base & __x, _List_node_base & __y) throw ();
    void _M_transfer (_List_node_base * const __first,
		      _List_node_base * const __last) throw ();
    void _M_reverse () throw ();
    void _M_hook (_List_node_base * const __position) throw ();
    void _M_unhook () throw ();
  };
  template < typename _Tp > struct _List_node:public _List_node_base
  {
    _Tp _M_data;
  };
  template < typename _Tp > struct _List_iterator
  {
    typedef _List_iterator < _Tp > _Self;
    typedef _List_node < _Tp > _Node;
    typedef ptrdiff_t difference_type;
    typedef std::bidirectional_iterator_tag iterator_category;
    typedef _Tp value_type;
    typedef _Tp *pointer;
    typedef _Tp & reference;
      _List_iterator ():_M_node ()
    {
    } explicit _List_iterator (_List_node_base * __x):_M_node (__x)
    {
    } reference operator* () const
    {
      return static_cast < _Node * >(_M_node)->_M_data;
    }
    pointer operator-> () const
    {
      return &static_cast < _Node * >(_M_node)->_M_data;
    }
    _Self & operator++ ()
    {
      _M_node = _M_node->_M_next;
      return *this;
    }
    _Self operator++ (int)
    {
      _Self __tmp = *this;
      _M_node = _M_node->_M_next;
      return __tmp;
    }
    _Self & operator-- ()
    {
      _M_node = _M_node->_M_prev;
      return *this;
    }
    _Self operator-- (int)
    {
      _Self __tmp = *this;
      _M_node = _M_node->_M_prev;
      return __tmp;
    }
    bool operator== (const _Self & __x) const
    {
      return _M_node == __x._M_node;
    }
    bool operator!= (const _Self & __x) const
    {
      return _M_node != __x._M_node;
    }
    _List_node_base *_M_node;
  };
  template < typename _Tp > struct _List_const_iterator
  {
    typedef _List_const_iterator < _Tp > _Self;
    typedef const _List_node < _Tp > _Node;
    typedef _List_iterator < _Tp > iterator;
    typedef ptrdiff_t difference_type;
    typedef std::bidirectional_iterator_tag iterator_category;
    typedef _Tp value_type;
    typedef const _Tp *pointer;
    typedef const _Tp & reference;
      _List_const_iterator ():_M_node ()
    {
    } explicit _List_const_iterator (const _List_node_base *
				     __x):_M_node (__x)
    {
    } _List_const_iterator (const iterator & __x):_M_node (__x._M_node)
    {
    } reference operator* () const
    {
      return static_cast < _Node * >(_M_node)->_M_data;
    }
    pointer operator-> () const
    {
      return &static_cast < _Node * >(_M_node)->_M_data;
    }
    _Self & operator++ ()
    {
      _M_node = _M_node->_M_next;
      return *this;
    }
    _Self operator++ (int)
    {
      _Self __tmp = *this;
      _M_node = _M_node->_M_next;
      return __tmp;
    }
    _Self & operator-- ()
    {
      _M_node = _M_node->_M_prev;
      return *this;
    }
    _Self operator-- (int)
    {
      _Self __tmp = *this;
      _M_node = _M_node->_M_prev;
      return __tmp;
    }
    bool operator== (const _Self & __x) const
    {
      return _M_node == __x._M_node;
    }
    bool operator!= (const _Self & __x) const
    {
      return _M_node != __x._M_node;
    }
    const _List_node_base *_M_node;
  };
  template < typename _Tp, typename _Alloc > class _List_base
  {
  protected:typedef typename _Alloc::template rebind < _List_node < _Tp >
      >::other _Node_alloc_type;
    typedef typename _Alloc::template rebind < _Tp >::other _Tp_alloc_type;
    struct _List_impl:public _Node_alloc_type
    {
      _List_node_base _M_node;
	_List_impl ():_Node_alloc_type (), _M_node ()
      {
      } _List_impl (const _Node_alloc_type & __a):_Node_alloc_type (__a),
	_M_node ()
      {
    }};
    _List_impl _M_impl;
    _List_node < _Tp > *_M_get_node ()
    {
      return _M_impl._Node_alloc_type::allocate (1);
    }
    void _M_put_node (_List_node < _Tp > *__p)
    {
      _M_impl._Node_alloc_type::deallocate (__p, 1);
  } public:typedef _Alloc allocator_type;
    _Node_alloc_type & _M_get_Node_allocator ()
    {
      return *static_cast < _Node_alloc_type * >(&this->_M_impl);
    }
    const _Node_alloc_type & _M_get_Node_allocator () const
    {
      return *static_cast < const _Node_alloc_type *>(&this->_M_impl);
    } _Tp_alloc_type _M_get_Tp_allocator () const
    {
      return _Tp_alloc_type (_M_get_Node_allocator ());
    }
    allocator_type get_allocator () const
    {
      return allocator_type (_M_get_Node_allocator ());
    }
    _List_base ():_M_impl ()
    {
      _M_init ();
    }
  _List_base (const allocator_type & __a):_M_impl (__a)
    {
      _M_init ();
    } ~_List_base ()
    {
      _M_clear ();
    } void _M_clear ();
    void _M_init ()
    {
      this->_M_impl._M_node._M_next = &this->_M_impl._M_node;
      this->_M_impl._M_node._M_prev = &this->_M_impl._M_node;
  }};
template < typename _Tp, typename _Alloc = std::allocator < _Tp > >class list:protected _List_base < _Tp,
    _Alloc
    >
  {
    typedef typename _Alloc::value_type _Alloc_value_type;
    typedef _List_base < _Tp, _Alloc > _Base;
    typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  public:typedef _Tp value_type;
    typedef typename _Tp_alloc_type::pointer pointer;
    typedef typename _Tp_alloc_type::const_pointer const_pointer;
    typedef typename _Tp_alloc_type::reference reference;
    typedef typename _Tp_alloc_type::const_reference const_reference;
    typedef _List_iterator < _Tp > iterator;
    typedef _List_const_iterator < _Tp > const_iterator;
    typedef std::reverse_iterator < const_iterator > const_reverse_iterator;
    typedef std::reverse_iterator < iterator > reverse_iterator;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef _Alloc allocator_type;
  protected:typedef _List_node < _Tp > _Node;
    using _Base::_M_impl;
    using _Base::_M_put_node;
    using _Base::_M_get_node;
    using _Base::_M_get_Tp_allocator;
    using _Base::_M_get_Node_allocator;
  public:iterator begin ()
    {
      return iterator (this->_M_impl._M_node._M_next);
    }
    const_iterator begin () const
    {
      return const_iterator (this->_M_impl._M_node._M_next);
    }
    iterator end ()
    {
      return iterator (&this->_M_impl._M_node);
    }
    void remove (const _Tp & __value);
    template < typename _Predicate > void remove_if (_Predicate);
    void _M_erase (iterator __position)
    {
      __position._M_node->_M_unhook ();
      _Node *__n = static_cast < _Node * >(__position._M_node);
      _M_get_Tp_allocator ().destroy (&__n->_M_data);
      _M_put_node (__n);
    } void _M_check_equal_allocators (list & __x)
    {
      if (std::__alloc_neq <
	  typename _Base::_Node_alloc_type >::
	  _S_do_it (_M_get_Node_allocator (), __x._M_get_Node_allocator ()));
    }
  };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp, typename _Alloc > void list < _Tp,
    _Alloc >::remove (const value_type & __value)
  {
    iterator __first = begin ();
    iterator __last = end ();
    iterator __extra = __last;
    while (__first != __last)
      {
	iterator __next = __first;
	++__next;
	if (*__first == __value)
	  {
	    if (&*__first != &__value)
	      _M_erase (__first);
	    else
	      __extra = __first;
	  }
	__first = __next;
      }
    if (__extra != __last)
      _M_erase (__extra);
  }
}

class Unit
{
public:int dummy;
};
class Building
{
public:__attribute__ ((transaction_callable)) void removeUnitFromInside (Unit *
								    unit);
    std::list < Unit * >unitsInside;
};
void
Building::removeUnitFromInside (Unit * unit)
{
  unitsInside.remove (unit);
}
