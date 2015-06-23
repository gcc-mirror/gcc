// PR c++/66542
// { dg-do compile { target c++11 } }

struct A
{
  A() {}
  ~A() = delete;		// { dg-message "declared here" }
};

static A a;			// { dg-error "deleted" }
