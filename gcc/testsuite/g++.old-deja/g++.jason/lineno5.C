// Build don't link: 
// GROUPS passed error-reporting
// Bug: incomplete instantiation messes with lineno
template <class T> class A;

int main()
{
  A<int> *p;
  undef1();// ERROR - 
}
