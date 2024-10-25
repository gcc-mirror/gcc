/* "WARNING: lto.exp does not support dg-additional-options" */
#pragma GCC diagnostic ignored "-Wtemplate-body"

namespace std
{
  typedef unsigned int size_t;
  typedef int ptrdiff_t;

}

namespace std __attribute__ ((__visibility__ ("default"))) {

  template<typename _Alloc>
    class allocator;

  template<class _CharT>
    struct char_traits;

  template<typename _CharT, typename _Traits = char_traits<_CharT>,
           typename _Alloc = allocator<_CharT> >
    class basic_string;

  template<> struct char_traits<char>;

  typedef basic_string<char> string;

  template<> struct char_traits<wchar_t>;

  typedef basic_string<wchar_t> wstring;
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  void
  __throw_bad_alloc(void) __attribute__((__noreturn__));
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _Iterator, typename _Container>
    class __normal_iterator;
}

namespace std __attribute__ ((__visibility__ ("default"))) {

  template<typename _Tp>
    inline _Tp*
    __addressof(_Tp& __r)
    {
      return reinterpret_cast<_Tp*>
 (&const_cast<char&>(reinterpret_cast<const volatile char&>(__r)));
    }
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<class _T1, class _T2>
    struct pair
    {
      typedef _T1 first_type;
      typedef _T2 second_type;

      _T1 first;
      _T2 second;

      pair()
      : first(), second() { }

      pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }
    };
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  struct input_iterator_tag { };

  struct output_iterator_tag { };

  struct forward_iterator_tag : public input_iterator_tag { };

  struct bidirectional_iterator_tag : public forward_iterator_tag { };

  struct random_access_iterator_tag : public bidirectional_iterator_tag { };
  template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t,
           typename _Pointer = _Tp*, typename _Reference = _Tp&>
    struct iterator
    {
      typedef _Category iterator_category;
      typedef _Tp value_type;
      typedef _Distance difference_type;
      typedef _Pointer pointer;
      typedef _Reference reference;
    };

  template<typename _Iterator>
    struct iterator_traits
    {
      typedef typename _Iterator::iterator_category iterator_category;
      typedef typename _Iterator::value_type value_type;
      typedef typename _Iterator::difference_type difference_type;
      typedef typename _Iterator::pointer pointer;
      typedef typename _Iterator::reference reference;
    };
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Iterator>
    class reverse_iterator
    : public iterator<typename iterator_traits<_Iterator>::iterator_category,
        typename iterator_traits<_Iterator>::value_type,
        typename iterator_traits<_Iterator>::difference_type,
        typename iterator_traits<_Iterator>::pointer,
                      typename iterator_traits<_Iterator>::reference>
    {
    protected:
      _Iterator current;
      typedef iterator_traits<_Iterator> __traits_type;
    };
}

struct _IO_FILE;

typedef struct _IO_FILE FILE;

typedef struct _IO_FILE __FILE;

typedef __builtin_va_list __gnuc_va_list;

typedef unsigned int size_t;
typedef unsigned int wint_t;

typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;


typedef __mbstate_t mbstate_t;

namespace std __attribute__ ((__visibility__ ("default"))) {
  using ::mbstate_t;
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  typedef long long streamoff;

  typedef ptrdiff_t streamsize;
  template<typename _StateT>
    class fpos
    {
    private:
      streamoff _M_off;
      _StateT _M_state;

    public:

      fpos()
      : _M_off(0), _M_state() { }
      fpos(streamoff __off)
      : _M_off(__off), _M_state() { }

      operator streamoff() const { return _M_off; }

    };

  typedef fpos<mbstate_t> streampos;

  typedef fpos<mbstate_t> wstreampos;
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _CharT>
    struct _Char_types
    {
      typedef unsigned long int_type;
      typedef std::streampos pos_type;
      typedef std::streamoff off_type;
      typedef std::mbstate_t state_type;
    };
  template<typename _CharT>
    struct char_traits
    {
      typedef _CharT char_type;
      typedef typename _Char_types<_CharT>::int_type int_type;
      typedef typename _Char_types<_CharT>::pos_type pos_type;
      typedef typename _Char_types<_CharT>::off_type off_type;
      typedef typename _Char_types<_CharT>::state_type state_type;

      static const char_type*
      find(const char_type* __s, std::size_t __n, const char_type& __a);
    };
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<class _CharT>
    struct char_traits : public __gnu_cxx::char_traits<_CharT>
    { };

  template<>
    struct char_traits<char>
    {
      typedef char char_type;
      typedef int int_type;
      typedef streampos pos_type;
      typedef streamoff off_type;
      typedef mbstate_t state_type;

      static const char_type*
      find(const char_type* __s, size_t __n, const char_type& __a)
      { return static_cast<const char_type*>(__builtin_memchr(__s, __a, __n)); }
  };
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {

  using std::size_t;
  using std::ptrdiff_t;
  template<typename _Tp>
    class new_allocator
    {
    public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp* pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp& reference;
      typedef const _Tp& const_reference;
      typedef _Tp value_type;

      new_allocator() throw() { }

      new_allocator(const new_allocator&) throw() { }

      template<typename _Tp1>
        new_allocator(const new_allocator<_Tp1>&) throw() { }

      ~new_allocator() throw() { }

      pointer
      allocate(size_type __n, const void* = 0)
      {
 if (__n > this->max_size())
   std::__throw_bad_alloc();

 return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
      }
      void
      deallocate(pointer __p, size_type)
      { ::operator delete(__p); }

      void
      destroy(pointer __p) { __p->~_Tp(); }
    };
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp>
    class allocator;

  template<typename _Tp>
    class allocator: public __gnu_cxx::new_allocator<_Tp>
    {
   public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp* pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp& reference;
      typedef const _Tp& const_reference;
      typedef _Tp value_type;

      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };

      allocator() throw() { }

      allocator(const allocator& __a) throw()
      : __gnu_cxx::new_allocator<_Tp>(__a) { }

      template<typename _Tp1>
        allocator(const allocator<_Tp1>&) throw() { }

      ~allocator() throw() { }
    };
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Arg, typename _Result>
    struct unary_function
    {
      typedef _Arg argument_type;
      typedef _Result result_type;
    };

  template<typename _Arg1, typename _Arg2, typename _Result>
    struct binary_function
    {
      typedef _Arg1 first_argument_type;
      typedef _Arg2 second_argument_type;
      typedef _Result result_type;
    };

  template<typename _Tp>
    struct less : public binary_function<_Tp, _Tp, bool>
    {
      bool
      operator()(const _Tp& __x, const _Tp& __y) const
      { return __x < __y; }
    };

  template<typename _Pair>
    struct _Select1st : public unary_function<_Pair,
           typename _Pair::first_type>
    {
      typename _Pair::first_type&
      operator()(_Pair& __x) const
      { return __x.first; }

      const typename _Pair::first_type&
      operator()(const _Pair& __x) const
      { return __x.first; }
    };
}

extern "C" {

typedef int __sig_atomic_t;

typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
typedef __sigset_t sigset_t;
}
typedef unsigned long int pthread_t;

typedef struct __pthread_internal_slist
{
  struct __pthread_internal_slist *__next;
} __pthread_slist_t;

typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;
    int __kind;

    unsigned int __nusers;
    __extension__ union
    {
      int __spins;
      __pthread_slist_t __list;
    };

  } __data;
  char __size[24];
  long int __align;
} pthread_mutex_t;

typedef unsigned int pthread_key_t;

typedef int pthread_once_t;

extern int pthread_once (pthread_once_t *__once_control,
    void (*__init_routine) (void)) __attribute__ ((__nonnull__ (1, 2)));

extern int pthread_mutex_lock (pthread_mutex_t *__mutex)
     throw () __attribute__ ((__nonnull__ (1)));

extern int pthread_mutex_unlock (pthread_mutex_t *__mutex)
     throw () __attribute__ ((__nonnull__ (1)));

typedef pthread_t __gthread_t;
typedef pthread_key_t __gthread_key_t;
typedef pthread_once_t __gthread_once_t;
typedef pthread_mutex_t __gthread_mutex_t;

static __typeof(pthread_once) __gthrw_pthread_once __attribute__ ((__weakref__("pthread_once")));

static __typeof(pthread_mutex_lock) __gthrw_pthread_mutex_lock __attribute__ ((__weakref__("pthread_mutex_lock")));

static __typeof(pthread_mutex_unlock) __gthrw_pthread_mutex_unlock __attribute__ ((__weakref__("pthread_mutex_unlock")));

static volatile int __gthread_active = -1;

static void
__gthread_trigger (void)
{
  __gthread_active = 1;
}

static inline int
__gthread_active_p (void)
{
  static pthread_mutex_t __gthread_active_mutex = { { 0, 0, 0, 0, 0, { 0 } } };
  static pthread_once_t __gthread_active_once = 0;

  int __gthread_active_latest_value = __gthread_active;

  if (__builtin_expect (__gthread_active_latest_value < 0, 0))
    {
      if (__gthrw_pthread_once)
 {
   __gthrw_pthread_mutex_lock (&__gthread_active_mutex);
   __gthrw_pthread_once (&__gthread_active_once, __gthread_trigger);
   __gthrw_pthread_mutex_unlock (&__gthread_active_mutex);
 }

      if (__gthread_active < 0)
 __gthread_active = 0;
      __gthread_active_latest_value = __gthread_active;
    }

  return __gthread_active_latest_value != 0;
}

typedef int _Atomic_word;

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {

  static inline _Atomic_word
  __exchange_and_add(volatile _Atomic_word* __mem, int __val)
  { return __sync_fetch_and_add(__mem, __val); }

  static inline void
  __atomic_add(volatile _Atomic_word* __mem, int __val)
  { __sync_fetch_and_add(__mem, __val); }
  static inline _Atomic_word
  __exchange_and_add_single(_Atomic_word* __mem, int __val)
  {
    _Atomic_word __result = *__mem;
    *__mem += __val;
    return __result;
  }

  static inline void
  __atomic_add_single(_Atomic_word* __mem, int __val)
  { *__mem += __val; }

  static inline _Atomic_word
  __attribute__ ((__unused__))
  __exchange_and_add_dispatch(_Atomic_word* __mem, int __val)
  {
    if (__gthread_active_p())
      return __exchange_and_add(__mem, __val);
    else
      return __exchange_and_add_single(__mem, __val);
  }

  static inline void
  __attribute__ ((__unused__))
  __atomic_add_dispatch(_Atomic_word* __mem, int __val)
  {
    if (__gthread_active_p())
      __atomic_add(__mem, __val);
    else
      __atomic_add_single(__mem, __val);
  }
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _CharT, typename _Traits, typename _Alloc>
    class basic_string
    {
      typedef typename _Alloc::template rebind<_CharT>::other _CharT_alloc_type;

    public:
      typedef _Traits traits_type;
      typedef typename _Traits::char_type value_type;
      typedef _Alloc allocator_type;
      typedef typename _CharT_alloc_type::size_type size_type;
      typedef typename _CharT_alloc_type::difference_type difference_type;
      typedef typename _CharT_alloc_type::reference reference;
      typedef typename _CharT_alloc_type::const_reference const_reference;
      typedef typename _CharT_alloc_type::pointer pointer;
      typedef typename _CharT_alloc_type::const_pointer const_pointer;
      typedef __gnu_cxx::__normal_iterator<pointer, basic_string> iterator;
      typedef __gnu_cxx::__normal_iterator<const_pointer, basic_string>
                                                            const_iterator;
      typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
      typedef std::reverse_iterator<iterator> reverse_iterator;

    private:
      struct _Rep_base
      {
 size_type _M_length;
 size_type _M_capacity;
 _Atomic_word _M_refcount;
      };

      struct _Rep : _Rep_base
      {

 typedef typename _Alloc::template rebind<char>::other _Raw_bytes_alloc;
 static const size_type _S_max_size;
 static const _CharT _S_terminal;

        static size_type _S_empty_rep_storage[];

        static _Rep&
        _S_empty_rep()
        {
   void* __p = reinterpret_cast<void*>(&_S_empty_rep_storage);
   return *reinterpret_cast<_Rep*>(__p);
 }

 _CharT*
 _M_refdata() throw()
 { return reinterpret_cast<_CharT*>(this + 1); }

 void
 _M_dispose(const _Alloc& __a)
 {
   if (__builtin_expect(this != &_S_empty_rep(), false))
     {
       ;
       if (__gnu_cxx::__exchange_and_add_dispatch(&this->_M_refcount,
        -1) <= 0)
  {
    ;
    _M_destroy(__a);
  }
     }
 }

 void
 _M_destroy(const _Alloc&) throw();

 _CharT*
 _M_refcopy() throw()
 {
   if (__builtin_expect(this != &_S_empty_rep(), false))
            __gnu_cxx::__atomic_add_dispatch(&this->_M_refcount, 1);
   return _M_refdata();
 }
      };

      struct _Alloc_hider : _Alloc
      {
 _Alloc_hider(_CharT* __dat, const _Alloc& __a)
 : _Alloc(__a), _M_p(__dat) { }

 _CharT* _M_p;
      };

    private:

      mutable _Alloc_hider _M_dataplus;

      _CharT*
      _M_data() const
      { return _M_dataplus._M_p; }

      _Rep*
      _M_rep() const
      { return &((reinterpret_cast<_Rep*> (_M_data()))[-1]); }

      void
      _M_leak_hard();

    public:

      ~basic_string()
      { _M_rep()->_M_dispose(this->get_allocator()); }

    public:

      allocator_type
      get_allocator() const
      { return _M_dataplus; }
  };
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  enum _Rb_tree_color { _S_red = false, _S_black = true };

  struct _Rb_tree_node_base
  {
    typedef _Rb_tree_node_base* _Base_ptr;
    typedef const _Rb_tree_node_base* _Const_Base_ptr;

    _Rb_tree_color _M_color;
    _Base_ptr _M_parent;
    _Base_ptr _M_left;
    _Base_ptr _M_right;

    static _Base_ptr
    _S_minimum(_Base_ptr __x)
    {
      while (__x->_M_left != 0) __x = __x->_M_left;
      return __x;
    }

    static _Const_Base_ptr
    _S_minimum(_Const_Base_ptr __x)
    {
      while (__x->_M_left != 0) __x = __x->_M_left;
      return __x;
    }

    static _Base_ptr
    _S_maximum(_Base_ptr __x)
    {
      while (__x->_M_right != 0) __x = __x->_M_right;
      return __x;
    }

    static _Const_Base_ptr
    _S_maximum(_Const_Base_ptr __x)
    {
      while (__x->_M_right != 0) __x = __x->_M_right;
      return __x;
    }
  };

  template<typename _Val>
    struct _Rb_tree_node : public _Rb_tree_node_base
    {
      typedef _Rb_tree_node<_Val>* _Link_type;
      _Val _M_value_field;
    };

  __attribute__ ((__pure__)) _Rb_tree_node_base*
  _Rb_tree_increment(_Rb_tree_node_base* __x) throw ();

  __attribute__ ((__pure__)) const _Rb_tree_node_base*
  _Rb_tree_increment(const _Rb_tree_node_base* __x) throw ();

  __attribute__ ((__pure__)) _Rb_tree_node_base*
  _Rb_tree_decrement(_Rb_tree_node_base* __x) throw ();

  __attribute__ ((__pure__)) const _Rb_tree_node_base*
  _Rb_tree_decrement(const _Rb_tree_node_base* __x) throw ();

  template<typename _Tp>
    struct _Rb_tree_iterator
    {
      typedef _Tp value_type;
      typedef _Tp& reference;
      typedef _Tp* pointer;

      typedef bidirectional_iterator_tag iterator_category;
      typedef ptrdiff_t difference_type;

      typedef _Rb_tree_iterator<_Tp> _Self;
      typedef _Rb_tree_node_base::_Base_ptr _Base_ptr;
      typedef _Rb_tree_node<_Tp>* _Link_type;

      _Rb_tree_iterator()
      : _M_node() { }

      explicit
      _Rb_tree_iterator(_Link_type __x)
      : _M_node(__x) { }

      bool
      operator==(const _Self& __x) const
      { return _M_node == __x._M_node; }

      bool
      operator!=(const _Self& __x) const
      { return _M_node != __x._M_node; }

      _Base_ptr _M_node;
  };

  template<typename _Tp>
    struct _Rb_tree_const_iterator
    {
      typedef _Tp value_type;
      typedef const _Tp& reference;
      typedef const _Tp* pointer;

      typedef _Rb_tree_iterator<_Tp> iterator;

      typedef bidirectional_iterator_tag iterator_category;
      typedef ptrdiff_t difference_type;

      typedef _Rb_tree_const_iterator<_Tp> _Self;
      typedef _Rb_tree_node_base::_Const_Base_ptr _Base_ptr;
      typedef const _Rb_tree_node<_Tp>* _Link_type;

      _Rb_tree_const_iterator()
      : _M_node() { }

      explicit
      _Rb_tree_const_iterator(_Link_type __x)
      : _M_node(__x) { }

      _Rb_tree_const_iterator(const iterator& __it)
      : _M_node(__it._M_node) { }

      pointer
      operator->() const
      { return std::__addressof(static_cast<_Link_type>
    (_M_node)->_M_value_field); }

      bool
      operator==(const _Self& __x) const
      { return _M_node == __x._M_node; }

      bool
      operator!=(const _Self& __x) const
      { return _M_node != __x._M_node; }

      _Base_ptr _M_node;
    };

  template<typename _Key, typename _Val, typename _KeyOfValue,
           typename _Compare, typename _Alloc = allocator<_Val> >
    class _Rb_tree
    {
      typedef typename _Alloc::template rebind<_Rb_tree_node<_Val> >::other
              _Node_allocator;

    protected:
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

      const _Node_allocator&
      _M_get_Node_allocator() const
      { return *static_cast<const _Node_allocator*>(&this->_M_impl); }

      allocator_type
      get_allocator() const
      { return allocator_type(_M_get_Node_allocator()); }

    protected:
      void
      _M_put_node(_Link_type __p)
      { _M_impl._Node_allocator::deallocate(__p, 1); }

      void
      _M_destroy_node(_Link_type __p)
      {
 get_allocator().destroy(std::__addressof(__p->_M_value_field));
 _M_put_node(__p);
      }

    protected:
      template<typename _Key_compare,
        bool _Is_pod_comparator = __is_pod(_Key_compare)>
        struct _Rb_tree_impl : public _Node_allocator
        {
   _Key_compare _M_key_compare;
   _Rb_tree_node_base _M_header;
   size_type _M_node_count;

 private:
   void
   _M_initialize()
   {
     this->_M_header._M_color = _S_red;
     this->_M_header._M_parent = 0;
     this->_M_header._M_left = &this->_M_header;
     this->_M_header._M_right = &this->_M_header;
   }
 };

      _Rb_tree_impl<_Compare> _M_impl;

    protected:

      _Link_type
      _M_begin()
      { return static_cast<_Link_type>(this->_M_impl._M_header._M_parent); }

      _Link_type
      _M_end()
      { return static_cast<_Link_type>(&this->_M_impl._M_header); }

      static _Link_type
      _S_left(_Base_ptr __x)
      { return static_cast<_Link_type>(__x->_M_left); }

      static _Link_type
      _S_right(_Base_ptr __x)
      { return static_cast<_Link_type>(__x->_M_right); }

      static const_reference
      _S_value(_Const_Base_ptr __x)
      { return static_cast<_Const_Link_type>(__x)->_M_value_field; }

      static const _Key&
      _S_key(_Const_Base_ptr __x)
      { return _KeyOfValue()(_S_value(__x)); }

    public:
      typedef _Rb_tree_iterator<value_type> iterator;
      typedef _Rb_tree_const_iterator<value_type> const_iterator;

    private:

      void
      _M_erase(_Link_type __x);

      iterator
      _M_lower_bound(_Link_type __x, _Link_type __y,
       const _Key& __k);

      const_iterator
      _M_lower_bound(_Const_Link_type __x, _Const_Link_type __y,
       const _Key& __k) const;

    public:

      ~_Rb_tree()
      { _M_erase(_M_begin()); }

      iterator
      end()
      { return iterator(static_cast<_Link_type>(&this->_M_impl._M_header)); }

      const_iterator
      end() const
      {
         return const_iterator(static_cast<_Const_Link_type>
         (&this->_M_impl._M_header));
      }

    public:
      iterator
      find(const key_type& __k);
    };

  template<typename _Key, typename _Val, typename _KeyOfValue,
           typename _Compare, typename _Alloc>
    void
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_erase(_Link_type __x)
    {

      while (__x != 0)
 {
   _M_erase(_S_right(__x));
   _Link_type __y = _S_left(__x);
   _M_destroy_node(__x);
   __x = __y;
 }
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
           typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue,
        _Compare, _Alloc>::iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    _M_lower_bound(_Link_type __x, _Link_type __y,
     const _Key& __k)
    {
      while (__x != 0)
 if (!_M_impl._M_key_compare(_S_key(__x), __k))
   __y = __x, __x = _S_left(__x);
 else
   __x = _S_right(__x);
      return iterator(__y);
    }

  template<typename _Key, typename _Val, typename _KeyOfValue,
           typename _Compare, typename _Alloc>
    typename _Rb_tree<_Key, _Val, _KeyOfValue,
        _Compare, _Alloc>::iterator
    _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::
    find(const _Key& __k)
    {
      iterator __j = _M_lower_bound(_M_begin(), _M_end(), __k);
      return (__j == end()
       || _M_impl._M_key_compare(__k,
     _S_key(__j._M_node))) ? end() : __j;
    }

}

namespace std {
  template <typename _Key, typename _Tp, typename _Compare = std::less<_Key>,
            typename _Alloc = std::allocator<std::pair<const _Key, _Tp> > >
    class map
    {
    public:
      typedef _Key key_type;
      typedef _Tp mapped_type;
      typedef std::pair<const _Key, _Tp> value_type;
      typedef _Compare key_compare;
      typedef _Alloc allocator_type;

    private:

      typedef typename _Alloc::template rebind<value_type>::other
        _Pair_alloc_type;

      typedef _Rb_tree<key_type, value_type, _Select1st<value_type>,
         key_compare, _Pair_alloc_type> _Rep_type;

      _Rep_type _M_t;

    public:

      typedef typename _Rep_type::iterator iterator;
      typedef typename _Rep_type::const_iterator const_iterator;

      map()
      : _M_t() { }

      const_iterator
      end() const
      { return _M_t.end(); }

      key_compare
      key_comp() const
      { return _M_t.key_comp(); }

      iterator
      find(const key_type& __x)
      { return _M_t.find(__x); }
    };
}

int main ()
{
  typedef std::map<int, std::string> Map;
  static Map m;

  Map::const_iterator it = m.find(0);
  if (it != m.end())
    std::string s = it->second;

  return 0;
}

