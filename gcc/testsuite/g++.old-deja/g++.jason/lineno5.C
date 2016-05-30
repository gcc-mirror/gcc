// { dg-do assemble  }
// GROUPS passed error-reporting
// Bug: incomplete instantiation messes with lineno
template <class T> class A;

int main()
{
  A<int> *p;
  undef1();// { dg-error "3:'undef1' was not declared" } 
}
