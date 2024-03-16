// PR c++/111895
// { dg-do compile { target c++11 } }

enum class o_field : unsigned char { no, yes, different_from_s };
struct fields {
  o_field o : 2;
};
bool func(fields f) { return static_cast<bool>(f.o); }
