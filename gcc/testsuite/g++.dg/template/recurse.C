// Test for handling of excessive template recursion.
// { dg-options "-ftemplate-depth-50 -O" }

template <int I> struct F
{
  int operator()()
    {
      F<I+1> f;			// { dg-error "incomplete type" "incomplete" }
				// { dg-bogus "exceeds maximum.*exceeds maximum" "exceeds" { xfail *-*-* } 8 }
                                // { dg-error "exceeds maximum" "exceeds" { xfail *-*-* } 8 }
      return f()*I;             // { dg-message "recursively instantiated" "recurse" }
    }
};

template <> struct F<52>
{
  int operator()() { return 0; }
};

int main ()
{
  F<1> f;
  return f();		// { dg-message "instantiated from here" "excessive recursion" }
}

// Ignore excess messages from recursion.
// { dg-prune-output "instantiated from 'int" }
