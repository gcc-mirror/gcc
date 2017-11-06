// PR debug/43290
// { dg-do compile }
// { dg-options "-O2" }
// { dg-options "-O3 -mavx -fPIC -mtune=core2" { target { { i?86-*-* x86_64-*-* } && { ilp32 && fpic } } } }

namespace std
{
  template <class> struct char_traits;
}
typedef struct { union { char __wchb[4]; }; } mbstate_t;
namespace std
{
  template <typename _StateT> struct fpos
  {
    long long _M_off;
    _StateT _M_state;
    fpos (long long):_M_off (), _M_state () { }
    _StateT state () { return _M_state; }
  };
  typedef fpos <mbstate_t> streampos;
}
namespace std
{
  template <> struct char_traits <char>
  {
    typedef streampos pos_type;
    typedef long long off_type;
    typedef mbstate_t state_type;
  };
}
struct pthread_mutex_t;
namespace
{
  enum _Ios_Openmode { _S_in = 3, _S_out };
  enum _Ios_Seekdir { _S_beg };
  struct ios_base
  {
    typedef _Ios_Openmode openmode;
    static const openmode in = _S_in;
    static const openmode out = _S_out;
    typedef _Ios_Seekdir seekdir;
    static const seekdir beg = _S_beg;
  };
  template < typename _CharT, typename > struct basic_streambuf
  {
    typedef _CharT char_type;
    char_type * _M_in_beg;
    char_type *eback () { return _M_in_beg; }
    char_type *gptr () { return 0; }
  };
}
namespace std
{
  typedef struct pthread_mutex_t __c_lock;
  template <typename> class __basic_file;
  template <> struct __basic_file <char>
  {
    __basic_file (__c_lock * = 0);
    bool is_open ();
  };
  template <typename _CharT, typename _Traits> struct basic_filebuf : public basic_streambuf <_CharT, _Traits>
  {
    typedef _CharT char_type;
    typedef _Traits traits_type;
    typedef typename traits_type::pos_type pos_type;
    typedef typename traits_type::off_type off_type;
    typedef __basic_file < char >__file_type;
    typedef typename traits_type::state_type __state_type;
    __file_type _M_file;
    char_type *_M_pback_cur_save;
    bool _M_pback_init;
    void _M_destroy_pback () throw ()
    {
	_M_pback_cur_save += this->gptr () != this->eback ();
	_M_pback_init = false;
    }
    bool is_open () throw () { return _M_file.is_open (); }
    pos_type seekpos (pos_type, ios_base::openmode = ios_base::in | ios_base::out);
    pos_type _M_seek (off_type, ios_base::seekdir, __state_type);
  };
  template <typename _CharT, typename _Traits>
  typename basic_filebuf <_CharT, _Traits>::pos_type
  basic_filebuf <_CharT, _Traits>::seekpos (pos_type __pos, ios_base::openmode)
  {
    pos_type __ret = (off_type ());
    if (this->is_open ())
      {
	_M_destroy_pback ();
	__ret = _M_seek (off_type (), ios_base::beg, __pos.state ());
      }
    return __ret;
  }
  template class basic_filebuf <char, char_traits <char> >;
}
