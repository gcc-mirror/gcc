// PR c++/29105

struct Observer
{
  template < typename T > void observeComponent ();
};

template < typename T >
struct TagFilter : Observer
{
  TagFilter ()
  {
    observeComponent < int > ();
  }
};

