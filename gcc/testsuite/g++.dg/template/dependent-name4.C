// { dg-do compile }

// Dependent arrays of invalid size cause template instantiation failure.

// We'll get an error message (duplicate matching templates) if the first
//  pattern is incorrectly allowed to match.

template<int M> void foobar (int (*) [M] = 0 );
template<int M> void foobar ( );

void fn (void)
{
  foobar<0>();
  foobar<-1>();
}
