#include "../../gcc.dg/analyzer/analyzer-decls.h"

class exception
{
};

class io_error : public exception
{
};

int __analyzer_inner ()
{
  try {
    throw io_error ();
  } catch (exception &exc) {
    __analyzer_dump_path (); // { dg-message "path" }
    return -1;
  }
  __analyzer_dump_path (); // { dg-bogus "path" }
  return 0;
}

void test ()
{
  __analyzer_eval (__analyzer_inner () == -1); /* { dg-warning "TRUE" } */
}
