// PR c++/4377

template < int I1, int I2 >
class unit
{
public:
  typedef unit<I1,I2> my_type;

  unit() {}
  unit( const unit<I1,I2>& ) {}

   template< int Q1, int Q2 >
   unit< I1 + Q1, I2 + Q2 > operator * ( const unit< Q1, Q2 >& rhs ) const {
     return unit< I1 + Q1, I2 + Q2 >();
   }
 
  template< int Q1, int Q2 >
  unit< I1 - Q1, I2 - Q2 > operator / ( const unit< Q1, Q2 >& rhs ) const {
    return unit< I1 - Q1, I2 - Q2 >();
  }
};

// specialization added to first test
//
template <>
class unit<0,0> {
public:
  typedef unit<0,0> my_type;

  unit() {}
  
   friend unit<0,0> operator*( const unit<0,0>& lhs, const unit<0,0>& rhs ) {
     return unit<0,0>();
   }
   friend unit<0,0> operator/( const unit<0,0>& lhs, const unit<0,0>& rhs ) {
     return unit<0,0>();
   }

};


int main()
{
  const unit<1,0> u1;
  const unit<2,0> u2;
 
  unit<-1,0> u3( u1 / u2 );
  unit< 3,0> u4( u1 * u2 );
}
