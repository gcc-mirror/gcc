#include "../../gcc.dg/analyzer/analyzer-decls.h"

int foo_count;

struct foo
{
  foo () __attribute__((noinline))
  {
    foo_count++;
  }  
  ~foo () __attribute__((noinline))
  {
    foo_count--;
  }
};

int main ()
{
  __analyzer_eval (foo_count == 0); // { dg-warning "TRUE" }
  {
    foo f;
    __analyzer_eval (foo_count == 1); // { dg-warning "TRUE" }
  }
  __analyzer_eval (foo_count == 0); // { dg-warning "TRUE" }
  return 0;
}
