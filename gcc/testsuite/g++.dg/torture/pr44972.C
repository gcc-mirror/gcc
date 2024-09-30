/* { dg-do compile } */
/* { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } } */

#include<cassert>
#include<new>
#include<utility>

namespace boost {

template<class T>
class optional;

class aligned_storage
{
	char data[ 1000 ];
  public:
    void const* address() const { return &data[0]; }
    void      * address()       { return &data[0]; }
} ;


template<class T>
class optional_base
{
  protected :
    optional_base(){}
    optional_base ( T const& val )
    {
      construct(val);
    }

    template<class U>
    void assign ( optional<U> const& rhs )
    {
      if (!is_initialized())
        if ( rhs.is_initialized() )
          construct(T());
    }

  public :

    bool is_initialized() const { return m_initialized ; }

  protected :

    void construct ( T const& val )
     {
       new (m_storage.address()) T(val) ;
     }

    T const* get_ptr_impl() const
    { return static_cast<T const*>(m_storage.address()); }

  private :

    bool m_initialized ;
    aligned_storage  m_storage ;
} ;


template<class T>
class optional : public optional_base<T>
{
    typedef optional_base<T> base ;

  public :

    optional() : base() {}
    optional ( T const& val ) : base(val) {}
    optional& operator= ( optional const& rhs )
      {
        this->assign( rhs ) ;
        return *this ;
      }

    T const& get() const ;

    T const* operator->() const { assert(this->is_initialized()) ; return this->get_ptr_impl() ; }

} ;


} // namespace boost


namespace std
{

  template<typename _Tp, std::size_t _Nm>
    struct array
    {
      typedef _Tp 	    			      value_type;
      typedef const value_type*			      const_iterator;

      value_type _M_instance[_Nm];

    };
}


class NT
{
  double _inf, _sup;
};


template < typename T > inline
std::array<T, 1>
make_array(const T& b1)
{
  std::array<T, 1> a = { { b1 } };
  return a;
}

class V
{
  typedef std::array<NT, 1>               Base;
  Base base;

public:
  V() {}
  V(const NT &x)
    : base(make_array(x)) {}

};

using boost::optional ;

optional< std::pair< NT, NT > >
  linsolve_pointC2() ;

optional< V > construct_normal_offset_lines_isecC2 ( )
{
  optional< std::pair<NT,NT> > ip;

  ip = linsolve_pointC2();

  V a(ip->first) ;
  return a;
}



