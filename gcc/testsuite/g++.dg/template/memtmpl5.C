// PR c++/79508

struct C
{
  template< void(*F)()> void set_default() { 	}	
};


template <class T> void random_positive()
{
}

template<class T> void initialize(T& x)
{
  x.template set_default<random_positive<T> >();
}

int main ()
{
  C x;
  initialize<C>(x);
}
