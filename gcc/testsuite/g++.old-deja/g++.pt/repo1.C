// { dg-do link }
// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

// Bug: g++ complains about duplicate explicit instantiations with -frepo.
// From Jason Merrill <jason@cygnus.com>

// Build then link:

template <class T> struct A {
  virtual ~A () { }
};

template <class T> void g (T t) { }

template class A<int>;

int main ()
{
  g (42);
}

// { dg-final { cleanup-repo-files } }
