// PR c++/116954
// { dg-additional-options -Wformat }

#ifndef WORKS
template<int N>
int fn(char (&buf)[N], const char fmt[], ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
#endif

template<int  N>
__attribute__ ((__format__ (__printf__, 2, 3)))
int fn(char (&)[N], const char [], ...)
{ return 0; }

int main()
{
  char buf[20];
  return fn(buf, "%s", 42); /* { dg-warning "Wformat" } */
}
