// Test non-type template argument folding.
// Origin: smacdonald@seimac.com

// { dg-do compile }

template < int I1, int I2 >
class unit
{
public:
  unit() {}
  unit( const unit<I1,I2>& ) {}
 
  template< int Q1, int Q2 >
  unit< I1 - Q1, I2 - Q2 > operator / ( const unit< Q1, Q2 >& rhs ) const {
    return unit< I1 - Q1, I2 - Q2 >();
  }
 
};
 
int main()
{
  const unit<1,0> u1;
  const unit<2,0> u2;
 
  unit<-1,0> u3( u1 / u2 );
}
