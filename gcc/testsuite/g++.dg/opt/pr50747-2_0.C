// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-options "-w -fpermissive -fno-implicit-templates -flto" }

namespace std {
    typedef long unsigned int size_t;
    template<typename _Alloc>     class allocator;
    template<class _CharT>     struct char_traits;
    template<typename _CharT, typename _Traits = char_traits<_CharT> >     class basic_ostream;
    template<typename _CharT, typename _Traits = char_traits<_CharT>,     typename _Alloc = allocator<_CharT> >     class basic_ostringstream;
}
extern "C++" {
    namespace std {
	class exception   {
	};
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class new_allocator     {
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _Tp>     class allocator: public __gnu_cxx::new_allocator<_Tp>     {
    public:       typedef size_t size_type;
		  template<typename _Tp1>         struct rebind         {
		      typedef allocator<_Tp1> other;
		  };
    };
    template<typename _CharT, typename _Traits, typename _Alloc>     class basic_string     {
	typedef typename _Alloc::template rebind<_CharT>::other _CharT_alloc_type;
	typedef typename _CharT_alloc_type::size_type size_type;
    private:       struct _Rep_base       {
		   };
		   struct _Rep : _Rep_base       {
		       _CharT*  _M_refdata() throw()  {
		       }
		   };
		   struct _Alloc_hider : _Alloc       {
		       _Alloc_hider(_CharT* __dat, const _Alloc& __a)  : _Alloc(__a), _M_p(__dat) {
		       }
		       _CharT* _M_p;
		   };
    private:       mutable _Alloc_hider _M_dataplus;
		   static _Rep&       _S_empty_rep()       {
		   }
    public:       basic_string()       : _M_dataplus(_S_empty_rep()._M_refdata(), _Alloc()) {
		  }
		  template<class _InputIterator>         basic_string(_InputIterator __beg, _InputIterator __end,        const _Alloc& __a = _Alloc());
		  static _CharT*       _S_construct(size_type __req, _CharT __c, const _Alloc& __a);
    };
    template<typename _CharT, typename _Traits, typename _Alloc>     inline basic_ostream<_CharT, _Traits>&     operator<<(basic_ostream<_CharT, _Traits>& __os,         const basic_string<_CharT, _Traits, _Alloc>& __str)     {
    }
    template<typename _CharT, typename _Traits, typename _Alloc>     template<typename _InputIterator>     basic_string<_CharT, _Traits, _Alloc>::     basic_string(_InputIterator __beg, _InputIterator __end, const _Alloc& __a)     : _M_dataplus(_S_construct(__beg, __end, __a), __a)     {
    };
    enum _Ios_Openmode     {
	_S_app = 1L << 0,       _S_ate = 1L << 1,       _S_bin = 1L << 2,       _S_in = 1L << 3,       _S_out = 1L << 4,       _S_trunc = 1L << 5,       _S_ios_openmode_end = 1L << 16     };
    class ios_base   {
    public:     class failure : public exception     {
		};
		typedef _Ios_Openmode openmode;
		static const openmode in = _S_in;
		static const openmode out = _S_out;
    };
    template<typename _CharT, typename _Traits>     class basic_streambuf     {
    public:       typedef _CharT char_type;
		  char_type*       egptr() const {
		  }
		  char_type*       pbase() const {
		  }
		  char_type*       pptr() const {
		  }
    };
    template<typename _CharT, typename _Traits>     class basic_ios : public ios_base     {
    };
    template<typename _CharT, typename _Traits>     class basic_ostream : virtual public basic_ios<_CharT, _Traits>     {
    };
    template<typename _CharT, typename _Traits, typename _Alloc>     class basic_stringbuf : public basic_streambuf<_CharT, _Traits>     {
    public:       typedef _CharT char_type;
		  typedef _Traits traits_type;
		  typedef basic_streambuf<char_type, traits_type> __streambuf_type;
		  typedef basic_string<char_type, _Traits, _Alloc> __string_type;
    protected:       ios_base::openmode _M_mode;
		     __string_type _M_string;
    public:       explicit       basic_stringbuf(ios_base::openmode __mode = ios_base::in | ios_base::out)       : __streambuf_type(), _M_mode(__mode), _M_string()       {
		  }
		  __string_type       str() const       {
		      __string_type __ret;
		      if (this->pptr())    {
			  if (this->pptr() > this->egptr())        __ret = __string_type(this->pbase(), this->pptr());
		      }
		  }
    };
    template <typename _CharT, typename _Traits, typename _Alloc>     class basic_ostringstream : public basic_ostream<_CharT, _Traits>     {
    public:       typedef _CharT char_type;
		  typedef _Traits traits_type;
		  typedef basic_string<_CharT, _Traits, _Alloc> __string_type;
		  typedef basic_stringbuf<_CharT, _Traits, _Alloc> __stringbuf_type;
		  typedef basic_ostream<char_type, traits_type> __ostream_type;
    private:       __stringbuf_type _M_stringbuf;
    public:       explicit       basic_ostringstream(ios_base::openmode __mode = ios_base::out)       : __ostream_type(), _M_stringbuf(__mode | ios_base::out)       {
		  }
		   __string_type       str() const       {
		       return _M_stringbuf.str();
		   }
    };
    template<typename _Tp> class complex;
    template<typename _Tp, typename _CharT, class _Traits>     basic_ostream<_CharT, _Traits>&     operator<<(basic_ostream<_CharT, _Traits>& __os, const complex<_Tp>& __x)     {
	basic_ostringstream<_CharT, _Traits> __s;
	return __os << __s.str();
    };
    template     basic_ostream<wchar_t, char_traits<wchar_t> >&     operator<<(basic_ostream<wchar_t, char_traits<wchar_t> >&,                const complex<long double>&);
}
