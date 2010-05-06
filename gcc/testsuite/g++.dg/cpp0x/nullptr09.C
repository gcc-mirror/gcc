// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test compare to literal 0

void fun()
{
  if( nullptr == 0 );
}
