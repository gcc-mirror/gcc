// { dg-do link }
// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }

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
