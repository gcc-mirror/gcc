// { dg-do assemble  }
// { dg-options "-ftemplate-depth-10" }
// Test for catching infinitely recursive instantiations.
// Origin: Jason Merrill <jason@redhat.com>


template <int i> void f()
{
  f<i+1>();			// { dg-error "" } excessive recursion
}

// We should never need this specialization because we should issue an
// error first about the recursive template instantions.  But, in case
// the compiler fails to catch the error, this will keep it from
// running forever instantiating more and more templates.
template <> void f<11>();

int main()
{
  f<0>();
}
