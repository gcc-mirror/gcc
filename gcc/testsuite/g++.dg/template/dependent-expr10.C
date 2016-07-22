// PR c++/70634

template < typename T >
bool foo ()
{
  const int i = sizeof (i) > 1 ? sizeof (T) : 0;
  return i > 0;
}
