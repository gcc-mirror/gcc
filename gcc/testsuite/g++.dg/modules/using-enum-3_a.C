// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi using_enum_3 }

export module using_enum_3;

export
struct text_encoding {
  enum class id { CP50220 };
  using enum id;
};
