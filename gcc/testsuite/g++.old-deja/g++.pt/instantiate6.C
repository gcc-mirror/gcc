// Build then link:
// Special g++ Options: -frepo

// Simplified from testcase by Erez Louidor Lior <s3824888@techst02.technion.ac.il>
// excess errors test - XFAIL *-*-*

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
