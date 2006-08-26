// PR c++/27547
// { dg-do compile }

int operator=(int);  // { dg-error "member function" }

void foo()
{
  operator=(0);  // { dg-error "not defined" }
}
