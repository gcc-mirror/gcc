// PR c++/49896

template<class C>
class test {
 protected:
  static const int versionConst = 0x80000000;
  enum { versionEnum = versionConst };
 public:
  int getVersion();
};

template<class C>
int test<C>::getVersion() {
  return versionEnum;
}

class dummy_class {};

int main() {
  test<dummy_class> t;
  return t.getVersion();
}
