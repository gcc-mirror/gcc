// PR c++/28267
// { dg-do compile }

struct A
{
  A();
  void* operator new(__SIZE_TYPE__, int = X);  // { dg-error "not declared" }
  void operator delete(void*, int);
};

void foo()
{
  new A;	// { dg-error "default argument" }
}
