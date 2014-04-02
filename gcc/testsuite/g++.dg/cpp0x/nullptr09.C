// { dg-do compile { target c++11 } }

// Test compare to literal 0

void fun()
{
  if( nullptr == 0 );
  decltype(nullptr) mynull = 0;
  if( mynull == 0 );
}
