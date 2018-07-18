// PR 79091 ICE mangling an unnamed enum in a tempate instantiation.

enum  {
  One = 1
};

template<int Options>
class Matrix {};

template<int Dim>
Matrix<Dim ? One : 0> *Bar ()
{
  return 0;
}

template<int Opt> 
Matrix<Opt> *Baz ()
{
  return 0;
}

bool Foo ()
{
  return Baz<1> () == Bar<1> ();
}
