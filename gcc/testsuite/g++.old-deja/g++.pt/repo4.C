// { dg-do link }
// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template <class T>
struct S {
  ~S ();
};

template <class T>
S<T>::~S () {}

int main ()
{
  S<int> s;
}

// { dg-final { cleanup-repo-files } }
