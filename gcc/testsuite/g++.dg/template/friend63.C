// PR c++/71738

template < class > struct S
{
  template < class > struct A
  { 
    template < class > struct B
    {
      template <class Z>
      void operator=(Z) { S::i = 0; }
    };
  };

  // Note that this friend declaration is useless, since nested classes are
  // already friends of their enclosing class.
  template < class X >
  template < class Y >
  template < class Z >
  friend void A < X >::B < Y >::operator= (Z);

private:
  static int i;
};

int main()
{
  S<int>::A<int>::B<int> b;
  b = 0;
}
