// { dg-options "-fabi-version=9 -Wabi" }
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
      static std::String Name;	// { dg-warning "mangled name" }
    }
  }
  void F2() {
    F();
  }
}
