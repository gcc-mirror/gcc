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
    throw io_error();
  } catch (exception &exc) {
    return -1;
  }
  __analyzer_dump_path (); // { dg-bogus "path" }
  return 0;
}

int test ()
{
  return __analyzer_inner (); // { dg-message "path" "PR analyzer/119697" { xfail *-*-* } }
}
