// Build don't link: 
//980519 bad error from nathan
//$ egcs -fhonor-std -nostdinc -c redef.C
//redef.C:56: redefinition of default argument for `class _Traits'

template<class _CharT> struct char_traits;
template<class _CharT> struct char_traits { };
template<> struct char_traits<char>;
template<> struct char_traits<char> { };

template<class _CharT, class _Traits = char_traits<_CharT> > class istreambuf_iterator;


template<class _CharT, class _Traits>
  class istreambuf_iterator
{
 public:
  typedef _Traits traits_type;
  class _Proxy;
 public:
  inline istreambuf_iterator() throw();
  inline istreambuf_iterator(const _Proxy& __p) throw();
};


template <class _CharT, class _Traits>
  class istreambuf_iterator<_CharT,_Traits>::_Proxy 
{
 public:
  _CharT operator*();

  //bug -g++  w/ decl "redef", no decl no prob.
  //ok -edg: no warnings
  friend class istreambuf_iterator;  //  XXX  OK?

  //bug -g++ w/ decl "redef", no decl no prob.
  //ok -edg: no warnings
  //friend class istreambuf_iterator<_CharT,_Traits>;

  //bug -g++ w/ decl "redef", no decl no prob.
  //ok -edg: declaration of "_CharT" and "_Traits" hides template parameter
  //template <class _CharT, class _Traits> friend class istreambuf_iterator; 

  //ok -g++
  //ok -edg
  //friend class istreambuf_iterator<_CharT>;

};



//explicit instantiation of a nested class
template class istreambuf_iterator<char, char_traits<char> >::_Proxy;

