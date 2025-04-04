// PR c++/119462
// { dg-additional-options "-fmodules -std=c++20 -Wno-global-module" }
// { dg-module-cmi M }

module;
struct exception_ptr {
  // implicitly noexcept and constexpr, but this isn't known yet
  friend bool operator==(const exception_ptr&, const exception_ptr&) = default;
};
export module M;
export import :part;
export using ::exception_ptr;
