// PR c++/124485
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi foo }

module;
namespace __cxxabiv1 {
  struct __class_type_info {};
}
struct type_info {
  void __do_upcast(__cxxabiv1::__class_type_info);
};
export module foo;
type_info t;
