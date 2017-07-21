// PR c++/33460

struct A
{
  struct
  {
    struct { static int i; }; // { dg-error "prohibits anonymous structs|non-static data members|unnamed class" }
    void foo() { i; } // { dg-error "can only have non-static data" }
  }; // { dg-error "prohibits anonymous structs" }
};
