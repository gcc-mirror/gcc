// PR c++/27547
// { dg-do compile }

int operator=(int);  // { dg-error "5:.int operator=\\(int\\). must be a non-static member function" }

void foo()
{
  operator=(0);  // { dg-error "not defined" }
}
