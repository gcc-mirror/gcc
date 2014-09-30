// Test for handling of excessive template recursion.
// { dg-options "-ftemplate-depth-50 -O" }

template <int I> struct F
{
  int operator()()
    {
      F<I+1> f;			// { dg-error "depth" }
      return f()*I;
    }
};

template <> struct F<52>
{
  int operator()() { return 0; }
};

int main ()
{
  F<1> f;
  return f();		// { dg-message "from here" }
}

// { dg-prune-output "compilation terminated" }
