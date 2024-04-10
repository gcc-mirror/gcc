// PR c++/110730
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

namespace std::filesystem {
  inline namespace __cxx11 __attribute__((__abi_tag__("cxx11", "foo"))) {
    struct path { };
  }
  inline path current_path() { return {}; };
}
