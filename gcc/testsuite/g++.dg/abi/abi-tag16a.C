// { dg-options "-fabi-version=9" }
// { dg-final { scan-assembler "_ZGVZN1N1FEvE4Name" } }
namespace std {
  __extension__ inline namespace __cxx11 __attribute__((abi_tag("cxx11"))) {
    struct String {
      String();
    };
  }
}
namespace N {
  inline void F() {
    {
      static std::String Name;
    }
  }
  void F2() {
    F();
  }
}
