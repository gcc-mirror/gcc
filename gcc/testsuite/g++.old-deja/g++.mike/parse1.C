// { dg-do assemble  }

enum { name1 };
struct name0 { };
struct name1 { };
struct derived1 : public name1 { };
struct derived2 : public name0, public name1 { };
