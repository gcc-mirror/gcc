// Test for catching infinitely recursive instantiations.
// Origin: Jason Merrill <jason@redhat.com>

template <int i> void f()
{
  f<i+1>();			// ERROR - excessive recursion
}

int main()
{
  f<0>();			// ERROR - starting here
}
