// PR c++/23840

#include <stdarg.h>
struct S 
{
  int f(int);
};
void f(int i, ...) 
{
  va_list ap;
  va_start (ap, i);
  va_arg (ap, S).f(0);
}
