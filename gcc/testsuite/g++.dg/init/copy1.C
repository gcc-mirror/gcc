// { dg-do compile }

class A {
public:
    A(){}
    A( const A& a ){}
};

class B  : public A
{
public:
  B( int& s) : s_(s){}
  int& s_;
};

int main()
{
  int i;
  B x1( i );
  B x2( x1 );
}
