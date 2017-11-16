// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

typedef long unsigned int size_t;
namespace std {
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<class _Sp, class _Tp>     struct __traitor     {
	enum {
	    __value = bool(_Sp::__value) || bool(_Tp::__value) };
    };
    template<typename _Tp>     struct __is_integer     {
	enum {
	    __value = 0 };
    };
    template<typename _Tp>     struct __is_floating     {
	enum {
	    __value = 0 };
    };
    template<typename _Tp>     struct __is_pointer     {
	enum {
	    __value = 0 };
    };
    template<typename _Tp>     struct __is_normal_iterator     {
	enum {
	    __value = 0 };
    };
    template<typename _Tp>     struct __is_arithmetic     : public __traitor<__is_integer<_Tp>, __is_floating<_Tp> >     {
    };
    template<typename _Tp>     struct __is_scalar     : public __traitor<__is_arithmetic<_Tp>, __is_pointer<_Tp> >     {
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<bool, typename>     struct __enable_if     {
    };
    template<typename _Tp>     struct __enable_if<true, _Tp>     {
	typedef _Tp __type;
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Iterator>     struct iterator_traits     {
    };
    template<typename _Tp>     struct iterator_traits<_Tp*>     {
	typedef _Tp value_type;
    };
    template<typename _Iterator, bool _HasBase>     struct _Iter_base     {
	typedef _Iterator iterator_type;
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Iterator>     struct _Niter_base     : _Iter_base<_Iterator, __is_normal_iterator<_Iterator>::__value>     {
    };
    template<typename _Iterator>     inline typename _Niter_base<_Iterator>::iterator_type     __niter_base(_Iterator __it)     {
    }
    template<typename _OutputIterator, typename _Size, typename _Tp>     inline typename     __gnu_cxx::__enable_if<!__is_scalar<_Tp>::__value, _OutputIterator>::__type     __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value)     {
	for (__decltype(__n + 0) __niter = __n;
	     __niter > 0;
	     --__niter, ++__first)  *__first = __value;
    }
    template<typename _OI, typename _Size, typename _Tp>     inline _OI     fill_n(_OI __first, _Size __n, const _Tp& __value)     {
	return _OI(std::__fill_n_a(std::__niter_base(__first), __n, __value));
    }
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     {
    public:
	typedef size_t size_type;
	typedef _Tp* pointer;
	~new_allocator() throw() {
	}
	pointer       allocate(size_type __n, const void* = 0)       {
	    return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
	}
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    public:
	template<typename _Tp1>         struct rebind         {
	    typedef allocator<_Tp1> other;
	};
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Alloc>   struct __alloc_traits   {
	typedef typename _Alloc::pointer pointer;
	template<typename _Tp>       struct rebind       {
	    typedef typename _Alloc::template rebind<_Tp>::other other;
	};
    };
}
class QString {
public:
    bool isEmpty() const;
};
class QObject {
};
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<bool _TrivialValueType>     struct __uninitialized_fill_n     {
	template<typename _ForwardIterator, typename _Size, typename _Tp>         static void         __uninit_fill_n(_ForwardIterator __first, _Size __n,    const _Tp& __x)         {
	    std::fill_n(__first, __n, __x);
	}
    };
    template<typename _ForwardIterator, typename _Size, typename _Tp>     inline void     uninitialized_fill_n(_ForwardIterator __first, _Size __n, const _Tp& __x)     {
	typedef typename iterator_traits<_ForwardIterator>::value_type  _ValueType;
	std::__uninitialized_fill_n<__is_trivial(_ValueType)>::  __uninit_fill_n(__first, __n, __x);
    }
    template<typename _ForwardIterator, typename _Size, typename _Tp,     typename _Tp2>     inline void     __uninitialized_fill_n_a(_ForwardIterator __first, _Size __n,         const _Tp& __x, allocator<_Tp2>&)     {
	std::uninitialized_fill_n(__first, __n, __x);
    }
    template<typename _Tp, typename _Alloc>     struct _Vector_base     {
	typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template         rebind<_Tp>::other _Tp_alloc_type;
	typedef typename __gnu_cxx::__alloc_traits<_Tp_alloc_type>::pointer         pointer;
	struct _Vector_impl       : public _Tp_alloc_type       {
	    pointer _M_start;
	    pointer _M_finish;
	    pointer _M_end_of_storage;
	    _Vector_impl(_Tp_alloc_type const& __a)  : _Tp_alloc_type(__a), _M_start(0), _M_finish(0), _M_end_of_storage(0)  {
	    }
	};
	typedef _Alloc allocator_type;
	_Tp_alloc_type&       _M_get_Tp_allocator()       {
	}
	_Vector_base(size_t __n, const allocator_type& __a)       : _M_impl(__a)       {
	    _M_create_storage(__n);
	}
	_Vector_impl _M_impl;
	pointer       _M_allocate(size_t __n)       {
	    return __n != 0 ? _M_impl.allocate(__n) : 0;
	}
	void       _M_create_storage(size_t __n)       {
	    this->_M_impl._M_start = this->_M_allocate(__n);
	    this->_M_impl._M_finish = this->_M_impl._M_start;
	}
    };
    template<typename _Tp, typename _Alloc = std::allocator<_Tp> >     class vector : protected _Vector_base<_Tp, _Alloc>     {
	typedef _Vector_base<_Tp, _Alloc> _Base;
	typedef _Tp value_type;
	typedef size_t size_type;
	typedef _Alloc allocator_type;
	using _Base::_M_get_Tp_allocator;
    public:
	explicit       vector(size_type __n, const value_type& __value = value_type(),       const allocator_type& __a = allocator_type())       : _Base(__n, __a)       {
	    _M_fill_initialize(__n, __value);
	}
	void       _M_fill_initialize(size_type __n, const value_type& __value)       {
	    std::__uninitialized_fill_n_a(this->_M_impl._M_start, __n, __value,           _M_get_Tp_allocator());
	}
    };
};
class QPaintDevice {
public:
    int width() const {
    }
    int height() const {
    }
};
class QImage : public QPaintDevice {
};
extern "C" {
    struct __jmp_buf_tag   {
    };
    typedef struct __jmp_buf_tag jmp_buf[1];
    extern int _setjmp (struct __jmp_buf_tag __env[1]) throw ();
    extern void longjmp (struct __jmp_buf_tag __env[1], int __val)      throw () __attribute__ ((__noreturn__));
}
typedef unsigned int png_uint_32;
typedef void * png_voidp;
typedef const char * png_const_charp;
extern "C" {
    typedef struct png_struct_def png_struct;
    typedef png_struct * png_structp;
    typedef void ( *png_error_ptr) (png_structp, png_const_charp);
    typedef void ( *png_longjmp_ptr) (jmp_buf, int);
    extern __attribute__((__malloc__)) png_structp ( png_create_write_struct) (png_const_charp user_png_ver, png_voidp error_ptr, png_error_ptr error_fn, png_error_ptr warn_fn)                   ;
    extern jmp_buf* ( png_set_longjmp_fn) (png_structp png_ptr, png_longjmp_ptr longjmp_fn, size_t jmp_buf_size)                                                      ;
}
class PngWriter : public QObject {
    const QImage *m_out_qimage;
    QString m_fname;
    bool writeQImageToPng();
};
bool PngWriter::writeQImageToPng() {
    png_uint_32 width = m_out_qimage->width();
    png_uint_32 height = m_out_qimage->height();
    if ( !m_fname.isEmpty() )  {
	std::vector<char> t(width * height * 4 + (width * height * 4) * 0.1);
    }
    png_structp png_ptr = png_create_write_struct     ("1.5.13", __null,   __null, __null);
    if (_setjmp ((*png_set_longjmp_fn((png_ptr), longjmp, sizeof (jmp_buf)))))  {
    }
}
