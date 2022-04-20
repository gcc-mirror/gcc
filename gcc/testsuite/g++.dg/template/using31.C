// PR c++/105223

struct ServiceReferenceBase {
  void operator=(int);
};

template<class>
struct ServiceReference : ServiceReferenceBase {
  void foo() { operator=(0); }
  using ServiceReferenceBase::operator=;
};

int main() {
  ServiceReference<int> sr;
  sr.foo();
}
