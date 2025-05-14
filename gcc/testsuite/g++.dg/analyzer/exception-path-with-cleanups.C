#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct foo
{
  foo (int x) : m_x (x) {}
  ~foo () __attribute__((nothrow));

  int m_x;  
};

int test (bool flag)
{
  foo outside (1);
  try
    {
      foo inside_try (2);
      if (flag)
	throw foo (3); // { dg-message "throwing exception of type 'foo' here\.\.\." }
    }
  catch (foo &f) // { dg-message "\.\.\.catching exception of type 'foo' here" }
    {
      __analyzer_dump_path (); // { dg-message "path" }
      __analyzer_eval (f.m_x == 3); // { dg-warning "TRUE" }
      return f.m_x;
    }
  return 0;
}
