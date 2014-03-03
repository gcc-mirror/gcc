// PR c++/30302

struct A
{
  struct { static int i; }; // { dg-error "prohibits anonymous structs|an anonymous struct|unnamed class" }
  void foo() { i; }
};

struct B
{
  union { static int i; }; // { dg-error "an anonymous union|member of a union|unnamed class" }
  void foo() { i; }
};
