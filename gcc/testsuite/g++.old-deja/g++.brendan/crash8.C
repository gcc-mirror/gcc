// { dg-do compile }
// GROUPS passed old-abort
template<int a, int b>
class Elvis // { dg-error "class Elvis" }
{
} ;

template<int a>
class Elvis<0> // { dg-error "wrong number of template arguments" }
{
  int geta() { return a ; }
} ;
