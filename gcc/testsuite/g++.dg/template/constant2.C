// PR c++/49896

template<class C>
class test {
 protected:
#if __SIZEOF_INT__ == 4
  static const int versionConst = 0x80000000;
#elif __SIZEOF_INT__ == 2
  static const int versionConst = 0x8000;
#elif __SIZEOF_INT__ == 1
  static const int versionConst = 0x80;
#endif
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
