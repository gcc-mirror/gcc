// PR c++/114683
// { dg-additional-options "-fmodules-ts -Wno-global-module" }

module;

namespace std
{
  inline namespace __cxx11
  {
    template <typename T>
    struct basic_string{};
  }
}

namespace foo {
  using std::basic_string;
}

export module std;

export namespace std
{
  using std::basic_string;
}
