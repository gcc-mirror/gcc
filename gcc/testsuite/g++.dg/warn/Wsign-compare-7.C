// PR c++/59231
// { dg-options "-Wsign-compare" }

template<class X, class Y>
bool equals(X x, Y y)
{
    return (x == y);		// { dg-warning "signed" }
}

int main()
{
  unsigned long x = 2;
  signed int y = 2;

  if(!equals (x, y))
    return 1;
  return 0;
}
