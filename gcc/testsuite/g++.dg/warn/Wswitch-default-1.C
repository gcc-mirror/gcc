// PR C++/21123
/* { dg-do compile } */
/* { dg-options "-Wswitch-default" } */


template <typename ArrayType>
void foo( )
{
    int i = 0;

  switch ( i ) /* { dg-bogus "switch missing default case" } */
  {
  case 9:
  default:
  break;
  }
}

void f()
{
  foo<int>();
}
