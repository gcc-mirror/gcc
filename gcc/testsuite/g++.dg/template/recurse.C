// Test for handling of excessive template recursion.
// { dg-options "-ftemplate-depth-50 -O" }

template <int I> struct F
{
  int operator()()
    {
      F<I+1> f;			// { dg-error "" "" }
      return f()*I;             // { dg-error "" "" }
    }
};

template <> struct F<52>
{
  int operator()() { return 0; }
};

int main ()
{
  F<1> f;
  return f();		// { dg-error "instantiate" "excessive recursion" }
}
