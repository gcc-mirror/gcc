// { dg-do link }
// { dg-options "-frepo" }
// Build then link:

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
