// { dg-do assemble  }
// Origin: Neil Booth, from bug report #36

template <typename t> class vect;
template <typename t> vect<t> operator-( const vect<t>&, const vect<t>& );

template <typename t>
class vect
{
public:
  vect( t a );

  vect( const vect<t>& v );
  ~vect();

  vect<t>& operator=( const vect<t>& v );
  vect<t>  operator-( void ) const;
  friend vect<t> (::operator- <>)( const vect<t>&, const vect<t>& );

private:
  t a_;
};

template <typename t> inline
vect<t>::vect( t a )
: a_(a)
{
}

template <typename t> inline
vect<t>::vect( const vect<t>& v )
: a_(v.a_)
{
}

template <typename t> inline
vect<t>::~vect()
{
}

template <typename t> inline vect<t>& 
vect<t>::operator=( const vect<t>& v )
{
   a_ = v.a_;
   return *this;
}

template <typename t> inline vect<t>
vect<t>::operator-( void ) const
{
  return vect<t>( -a_ );
}

template <typename t> inline vect<t>
operator-( const vect<t>& u, const vect<t>& v )
{
  return vect<t>( u.a_ - v.a_ );
}

int
main( void )
{
  vect<double> a( 1.0 ), b( 0.0 );
  b = -a;
}
