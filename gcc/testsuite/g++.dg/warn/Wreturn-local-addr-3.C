// PR c++/51270
// { dg-options "-O2" }

const int& f(long l)
{
  const int& i = l;
  return i;  // { dg-warning "address of local variable" }
}
