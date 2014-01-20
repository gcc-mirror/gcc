// PR c++/59269
// { dg-require-effective-target c++11 }

union U
{
  int& i = 0;  // { dg-error "reference" }
};

void foo()
{
  U();
}
