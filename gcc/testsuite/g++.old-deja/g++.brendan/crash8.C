// Build don't link: 
// GROUPS passed old-abort
template<int a, int b>
class Elvis
{// ERROR - in template.*
} ;

template<int a>
class Elvis<0>
{ // ERROR - incorrect number of parameters
   int geta() { return a ; }
} ;
