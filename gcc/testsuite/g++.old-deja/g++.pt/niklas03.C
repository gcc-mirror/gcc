// { dg-do assemble  }

template <class A> struct X {
  A operator[] (int);
};

template <class A> A X<A>::operator[] (int i)
{
  return A();	// { dg-bogus "" } 
}

X<int> x;
