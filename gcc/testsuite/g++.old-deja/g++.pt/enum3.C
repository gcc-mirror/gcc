struct S { enum en { s0, s1, s2 }; };

template<typename S::en e>
int val( )
{
  return e;
}


int main()
{
  return val<S::s0>( );
}
 
