// { dg-additional-options "-fmodule-header -std=c++20" }
// { dg-module-cmi {} }

struct exception_ptr {
  friend bool operator==(const exception_ptr&, const exception_ptr&) = default;
};
