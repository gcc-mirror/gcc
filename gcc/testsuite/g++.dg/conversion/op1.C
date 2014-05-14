class C
{
  template<typename U> 
  operator U();			// { dg-message "note" }
};

int fn (C c) 
{ 
  return C::operator float(c); // { dg-error "operator float.C" }
}
