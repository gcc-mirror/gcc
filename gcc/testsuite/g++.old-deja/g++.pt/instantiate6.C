// { dg-do link }
// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }

// Simplified from testcase by Erez Louidor Lior <s3824888@techst02.technion.ac.il>

template <class T>
class foo{
public:
  void g();
  void h();
};

template <class T>
void foo<T>::g() {
  h();
}

template <class T>
void foo<T>::h() {
}

int main() {
  foo<int> f;
  f.g();
}

// { dg-final { cleanup-repo-files } }
