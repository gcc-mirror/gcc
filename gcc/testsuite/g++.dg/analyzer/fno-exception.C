/* { dg-additional-options "-fno-exceptions" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void do_something (); 

int test ()
{
  do_something ();
  __analyzer_dump_path (); // { dg-message "path" }
  return 0;
}
