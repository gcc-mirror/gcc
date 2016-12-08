// PR optimization/12965
// Origin: <qboosh@pld-linux.org>
// Reduced testcase: Falk Hueffner <falk@debian.org>

// This ICEd on Alpha because the reload pass emitted save/restore
// insns around a no-return call.

// { dg-do compile }
// { dg-options "-O2" }

template <typename _Alloc> class allocator;
template <class _CharT> struct char_traits;
template <typename _CharT,
	  typename _Traits = char_traits<_CharT>,
	  typename _Alloc = allocator<_CharT> >
class basic_string;
typedef basic_string<char> string;

static inline int __exchange_and_add(volatile int * __mem, int __val) {
    int __result;
    asm("" : "=&r"(__result));
    return __result;
}

template<typename _Tp> struct allocator {
    allocator() throw() { }
    allocator(const allocator &) throw() {}
};

template<typename _CharT, typename _Traits, typename _Alloc>
struct basic_string {
    typedef _Alloc allocator_type;
    struct _Rep {
	int _M_references;
	void _M_dispose(const _Alloc & __a) {
	    if (__exchange_and_add(&_M_references, -1) <= 0)
		_M_destroy(__a);
	} void _M_destroy(const _Alloc &) throw();
    };
    struct _Alloc_hider : _Alloc {
	_CharT *_M_p;
    };
    mutable _Alloc_hider _M_dataplus;
    _CharT *_M_data() const { return _M_dataplus._M_p; }
    _Rep *_M_rep() const {
	return &((reinterpret_cast<_Rep *>(_M_data()))[-1]);
    }
    basic_string();
    basic_string(const _CharT * __s, const _Alloc & __a = _Alloc());
    ~basic_string() {
	_M_rep()->_M_dispose(this->get_allocator());
    }
    allocator_type get_allocator() const { return _M_dataplus; }
};

struct Egeneric {
    void stack(const string & passage, const string & message = "") { }
};

struct infinint {
    void detruit()
#if __cplusplus <= 201402L
    throw(Egeneric)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
    template<class T> void infinint_from(T a)
#if __cplusplus <= 201402L
    throw(Egeneric)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
    infinint(long a = 0)
#if __cplusplus <= 201402L
    throw(Egeneric)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    {
	try {
	    infinint_from(a);
	} catch(Egeneric& e) {
	    e.stack("infinint::infinint", "long");
	}
    }
    ~infinint()
#if __cplusplus <= 201402L
    throw(Egeneric)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#else
    noexcept(false)
#endif
    {
	try {
	    detruit();
	} catch(Egeneric& e) { }
    }
};

struct inode {
    string x;
    infinint a, c;
    infinint ea_offset;
    inode();
};

inode::inode()
{
    ea_offset = 0;
}
