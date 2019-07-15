// { dg-do link  }
// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

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
