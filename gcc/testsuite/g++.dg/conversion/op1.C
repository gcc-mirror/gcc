class C
{
  template<typename U> 
  operator U();
};

int fn (C c) 
{ 
  return C::operator float(c); // { dg-error "operator U" }
}
