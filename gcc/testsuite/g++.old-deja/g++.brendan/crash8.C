// { dg-do assemble  }
// GROUPS passed old-abort
template<int a, int b>
class Elvis
{ // { dg-error "" } in template.*
} ;

template<int a>
class Elvis<0>
{ // { dg-error "" } incorrect number of parameters
  int geta() { return a ; }
} ;
