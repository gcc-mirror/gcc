// PR c++/54416

template < typename T > struct foo;
template <> struct foo < int >;
template < typename T > struct bar
{
  template <> struct foo < int > // { dg-error "non-namespace scope" }
  {
    void baz ();
  };
};
void foo < int >::baz () { }

// Don't be picky about error-recovery.
// { dg-prune-output "." }
