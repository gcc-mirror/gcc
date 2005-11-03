// PR c++/22434
// { dg-options "" }

struct A
{
  A(void*);
  ~A();
};

void foo(const int i, bool b)
{
  b ? A(0) : i; // { dg-error "conversion|initializing" } 
}
