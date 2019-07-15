// PR c++/17163
// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template <int __inst>
struct __Atomicity_lock
{
  static unsigned char _S_atomicity_lock;
};
template <int __inst>
unsigned char __Atomicity_lock<__inst>::_S_atomicity_lock = 0;
template unsigned char __Atomicity_lock<0>::_S_atomicity_lock;

int main () {
}

// { dg-final { cleanup-repo-files } }
