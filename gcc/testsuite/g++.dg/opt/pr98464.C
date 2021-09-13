// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O3 -fno-tree-dce" }

template < typename, typename, template < typename > class _Op,            typename... _Args > struct __detector {
      using type = _Op< _Args... >;
    };
     template < typename _Default, template < typename > class _Op,            typename... _Args > using __detected_or = __detector< _Default, void, _Op, _Args... >;
     template < typename _Default, template < typename > class _Op,            typename... _Args > using __detected_or_t = typename __detected_or< _Default, _Op, _Args... >::type;
     template < typename, typename > struct __replace_first_arg;
     template < template < typename > class _Template, typename _Up, typename _Tp,            typename... _Types > struct __replace_first_arg< _Template< _Tp, _Types... >, _Up > {
      using type = _Template< _Up >;
    };
     template < class > class min_pointer;
     class MoveOnly;
     struct pointer_traits {
      template < typename _Up >   using rebind = typename __replace_first_arg< min_pointer< int >, _Up >::type;
    };
     template < typename _Iterator > class __normal_iterator {
    public:   __normal_iterator(_Iterator);
    };
     struct __allocator_traits_base {
      template < typename _Tp > using __pointer = typename _Tp::pointer;
    };
     template < typename _Alloc > struct allocator_traits : __allocator_traits_base {
      typedef typename _Alloc::value_type value_type;
      using pointer = __detected_or_t< value_type, __pointer, _Alloc >;
      template < typename _Tp > struct _Ptr {
       using type = pointer_traits::rebind< _Tp >;
     };
      using const_pointer = typename _Ptr< value_type >::type;
      using size_type = int;
      static pointer allocate(_Alloc __a, size_type __n) {
       return __a.allocate(__n);
     }
    };
     template < typename _ForwardIterator, typename _Allocator > void _Destroy(_ForwardIterator __first, _ForwardIterator __last, _Allocator) {
      for (;
    __first != __last;
    ++__first)     ;
    }
     template < typename _InputIterator, typename _ForwardIterator,            typename _Allocator > _ForwardIterator __uninitialized_copy_a(_InputIterator, _ForwardIterator,                                         _Allocator);
     template < typename _InputIterator, typename _ForwardIterator,            typename _Allocator > _ForwardIterator __uninitialized_move_if_noexcept_a(_InputIterator __last,                                                     _ForwardIterator __result,                                                     _Allocator __alloc) {
      return __uninitialized_copy_a(__last, __result, __alloc);
    }
     template < typename _ForwardIterator, typename _Size, typename _Allocator > _ForwardIterator __uninitialized_default_n_a(_ForwardIterator __first,                                              _Size __n, _Allocator) {
      for (;
    __n;
    --__n, ++__first)     ;
      return __first;
    }
     template < typename _Alloc > struct _Vector_base {
      typedef _Alloc _Tp_alloc_type;
      typedef typename _Tp_alloc_type ::pointer pointer;
      struct _Vector_impl_data {
       pointer _M_start;
       pointer _M_finish;
       pointer _M_end_of_storage;
     };
      struct _Vector_impl : _Tp_alloc_type, _Vector_impl_data {
       _Vector_impl(_Tp_alloc_type) {
 }
     };
      _Vector_base(long __n, _Alloc __a) : _M_impl(__a) {
       _M_impl._M_end_of_storage = _M_impl._M_start + __n;
     }
      _Vector_impl _M_impl;
      pointer _M_allocate(long __n) {
       return __n ? allocator_traits< _Tp_alloc_type >::allocate(_M_impl, __n)                : pointer();
     }
    };
     template < typename, typename _Alloc > class vector : _Vector_base< _Alloc > {
    public:   typedef typename _Alloc::pointer pointer;
      typedef __normal_iterator<       typename allocator_traits< _Alloc >::const_pointer >       const_iterator;
      typedef _Alloc allocator_type;
      vector(long __n, allocator_type __a = allocator_type())       : _Vector_base< _Alloc >(__n, __a) {
       this->_M_impl._M_finish =         __uninitialized_default_n_a(this->_M_impl._M_start, __n, 0);
     }
      ~vector() {
   _Destroy(this->_M_impl._M_start, this->_M_impl._M_finish, 0);
   }
      const_iterator cbegin() {
   return this->_M_impl._M_start;
   }
      typename _Alloc::value_type operator[](long) {
       return *this->_M_impl._M_start;
     }
      void insert(const_iterator, MoveOnly &&) {
       if (this->_M_impl._M_finish != this->_M_impl._M_end_of_storage)       ;
       else       _M_realloc_insert();
     }
      template < typename... > void _M_realloc_insert();
    };
     template < typename _Tp, typename _Alloc > template < typename... > void vector< _Tp, _Alloc >::_M_realloc_insert() {
      long __trans_tmp_6 = this->_M_impl._M_finish - this->_M_impl._M_start;
      pointer __old_start = this->_M_impl._M_start;
      pointer __old_finish = this->_M_impl._M_finish;
      pointer __new_start(this->_M_allocate(__trans_tmp_6));
      pointer __new_finish =       __uninitialized_move_if_noexcept_a(__old_finish, __new_finish, 0);
      _Destroy(__old_start, __old_finish, 0);
      this->_M_impl._M_start = __new_start;
      this->_M_impl._M_finish = __new_finish;
    }
     class MoveOnly {
      int data_;
     public:   bool operator==(MoveOnly) {
   return data_;
   }
    };
     void __assert_fail();
     template < class T > class min_pointer {
      T *ptr_;
      min_pointer(T *p) : ptr_(p) {
  }
     public:   min_pointer() = default;
      T operator*() {
   return *ptr_;
   }
      void operator++() {
   ++ptr_;
   }
      void operator+=(long n) {
   ptr_ += n;
   }
      min_pointer operator+(long n) {
       min_pointer tmp(*this);
       tmp += n;
       return tmp;
     }
      friend long operator-(min_pointer x, min_pointer y) {
       return x.ptr_ - y.ptr_;
     }
      friend bool operator==(min_pointer x, min_pointer y) {
       return x.ptr_ == y.ptr_;
     }
      friend bool operator!=(min_pointer x, min_pointer y) {
   return !(x == y);
   }
      friend class min_allocator;
    };
     class min_allocator {
    public:   typedef MoveOnly value_type;
      typedef min_pointer< MoveOnly > pointer;
      pointer allocate(long) {
       return static_cast< MoveOnly * >(operator new(sizeof(MoveOnly)));
     }
    };
     int main() {
      vector< int, min_allocator > v(100);
      v.insert(v.cbegin(), MoveOnly());
      int j = 0;
      for (;
    j < 10;
    ++j)     v[j] == MoveOnly() ? void() : __assert_fail();
    }
