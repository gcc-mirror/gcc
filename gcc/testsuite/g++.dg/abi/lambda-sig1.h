template<typename T, unsigned I> struct X 
{
  void Fn () 
  {
    {
      auto v1 = []<typename U> (U) {};
      auto v2 = [] (T) {};
      auto v3 = []<typename U> (T, U) {};

      v1 (1);
      v2 (1);
      v3 (1, 2);
    }
    if constexpr (I) 
      {
	auto v1 = []<typename U> (U) {};
	auto v2 = [] (T) {};
	auto v3 = []<typename U> (T, U) {};

	v1 (1);
	v2 (1);
	v3 (1, 2);
      }
    {
      auto v1 = []<typename U> (U) {};
      auto v2 = [] (T) {};
      auto v3 = []<typename U> (T, U) {};

      v1 (1);
      v2 (1);
      v3 (1, 2);
    }
    
  };
};

void f (X<int, 0> &x, X<float, 0> &y, X<float, 1> &z)
{
  x.Fn ();
  y.Fn ();
  z.Fn ();
}
