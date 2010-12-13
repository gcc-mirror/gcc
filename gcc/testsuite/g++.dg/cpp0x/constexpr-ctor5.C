// PR c++/46877
// { dg-options -std=c++0x }

struct new_allocator
{
  constexpr new_allocator ();
};

struct string
{
  constexpr string ()
  {
  }
  new_allocator a;
};

struct pair
{
  const string first;
  constexpr pair ()
  {
  }
};

constexpr
new_allocator::new_allocator ()
{
}

pair p;
