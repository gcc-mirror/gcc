#include "../../gcc.dg/analyzer/analyzer-decls.h"

class exception
{
};

class io_error : public exception
{
};

int test ()
{
  try {
    throw io_error();
  } catch (exception &exc) {
    __analyzer_dump_path (); // { dg-message "path" "PR analyzer/119697" { xfail *-*-* } }
    return -1;
  }
  __analyzer_dump_path (); // { dg-bogus "path" }
  return 0;
}
