// PR c++/27547
// { dg-do compile }

int operator=(int);  // { dg-error "5:.int operator=\\(int\\). must be a nonstatic member function" }

void foo()
{
  operator=(0);  // { dg-error "not defined" }
}
