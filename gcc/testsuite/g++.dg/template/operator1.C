class test
{
public:
 float operator[]( int index )
 {
  return testFloat[index];
 }
private:
 float testFloat[3];
};

template < class typeA > float
operator*(
 typeA a,
 float b
)
{
 return a[0] * b;
}

template < class typeB > float
operator*(
 float a,
 typeB b
)
{
 return a * b[0];
}

template < class typeA, class typeB > float
operator*(
 typeA a,
 typeB b
)
{
 return a[0] * b[0];
}

int main( void )
{
 test aTest;
 float bTest;
 float result;

 result = aTest * bTest;
 result = bTest * aTest;

 return 0;
}
