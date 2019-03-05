// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options {{-fPIC -flto -Wno-return-type}} }
// { dg-extra-ld-options "-fPIC -r -nostdlib" }

namespace std __attribute__ ((__visibility__ ("default")))
{
  template<class _CharT>     struct char_traits;
  template<typename _CharT, typename _Traits = char_traits<_CharT> >
      class basic_ostream;
  template<typename _CharT, typename _Traits = char_traits<_CharT> >
      class istreambuf_iterator;
  typedef basic_ostream<char> ostream;
  template<typename _CharT, typename _InIter = istreambuf_iterator<_CharT> >
      class num_get;
  class locale   {
      class facet;
  };
  class locale::facet   {
  };
  enum _Ios_Iostate { _S_beg = 0, _S_cur = 1, _S_end = 2,
      _S_ios_seekdir_end = 1L << 16     };
  class ios_base   {
  public:
      typedef _Ios_Iostate iostate;
  };
  template<typename _CharT, typename _InIter>
      class num_get : public locale::facet     {
	  typedef _InIter iter_type;
	  template<typename _ValueT> iter_type
	      _M_extract_int(iter_type, iter_type, ios_base&,
			     ios_base::iostate&, _ValueT&) const;
	  virtual iter_type
	      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, bool&) const;
      };
  extern template class num_get<char>;
  template<typename _CharT, typename _Traits>
      class basic_ios : public ios_base     {
	  typedef num_get<_CharT, istreambuf_iterator<_CharT, _Traits> >
	      __num_get_type;
	  const __num_get_type* _M_num_get;
      };
  template<typename _CharT, typename _Traits>
      class basic_ostream : virtual public basic_ios<_CharT, _Traits>     {
      public:
	  typedef basic_ostream<_CharT, _Traits> __ostream_type;
	  __ostream_type&       operator<<(double __f)       { }
      };
  typedef double Real;
  class Vector {
  public:
      Real operator[](int n) const    { }
  };
  std::ostream& operator<<(std::ostream& s, const Vector& vec)
    {
      int i;
      s << vec[i] << ')';
    }
}
