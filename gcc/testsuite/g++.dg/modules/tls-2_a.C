// PR c++/120363
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

export struct test {
  static inline const int& get_instance() {
    return instance;
  }
  static thread_local int instance;
};
