// { dg-do link  }
// { dg-options "-frepo" }
// Test that collect2 isn't confused by GNU ld's "In function `foo':" message.
// Contributed by Jason Merrill <jason@cygnus.com>

// Build then link:

template <class T>
T f (T t)
{
  return t;
}

template <class T>
T g (T t)
{
  return f (t);
}

int main ()
{
  int i = g (42);
}

// { dg-final { cleanup-repo-files } }
