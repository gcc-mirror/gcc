// { dg-do compile }

template < typename = void >
struct X { } ;
struct Y
{
  Y () ;
} ;
template < typename = X < > >
struct T
{
  void f ()
    {
      f () ;
    }
} ;
struct S
{
  S ( X < > = X < > ()) ;
  ~S ()
    {
      T < > () . f () ;
    }
} ;
struct Z
{
  S s ;
  Y y ;
} a ;

