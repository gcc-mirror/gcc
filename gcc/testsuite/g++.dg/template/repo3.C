// { dg-options "-frepo -DF='a'" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template <typename A, typename B> void f () {}
template <typename A, typename B> void g () { f<int,int>(); }
int main () { g<int,int>(); }

char c = F;

// { dg-final { cleanup-repo-files } }
