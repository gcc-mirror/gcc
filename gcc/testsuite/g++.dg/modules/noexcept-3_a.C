// PR c++/119462
// { dg-additional-options "-fmodules -std=c++20 -Wno-global-module" }
// { dg-module-cmi M:part }

module;
struct exception_ptr {
  // implicitly noexcept and constexpr
  friend bool operator==(const exception_ptr&, const exception_ptr&) = default;
};
export module M:part;
export template <typename = int> void enqueue() {
  exception_ptr e;
  e == e;
}
