// PR c++/112820
// { dg-additional-options "-fmodules-ts -g" }
// { dg-module-cmi io }

export module io;

export struct error {
  virtual const char* what() const noexcept;
};
