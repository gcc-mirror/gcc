// PR c++/23789

template <int W> struct X { 
  template <int W2> 
  X< (W+(W&&W) > 1 ? W+(W&&W) : 1)+1> 
  operator + (const X<W2>&) const; 
}; 
 
template <int dummy> void foo() 
{ 
  X<6> A,B; 
  A + B; 
}
