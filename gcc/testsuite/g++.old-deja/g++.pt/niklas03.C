// Build don't link: 

template <class A> struct X {
  A operator[] (int);
};

template <class A> A X<A>::operator[] (int i)
{
  return A();	// gets bogus error
}

X<int> x;
