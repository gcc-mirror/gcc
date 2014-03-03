// PR c++/33460

struct A
{
  struct
  {  // { dg-error "anonymous struct cannot have function members" }
    struct { static int i; }; // { dg-error "prohibits anonymous structs|non-static data members|unnamed class" }
    void foo() { i; }
  }; // { dg-error "prohibits anonymous structs" }
};
