/* Verify that the analyzer is silent on correct usage of std::string,
   using <string>.  */

// { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } }

/* Leaving assertions at default: we seem to need assertions at -O0
   for the test to pass.  */

// { dg-additional-options "--param=analyzer-supernode-explosion-factor=10" }

#include "../../../gcc.dg/analyzer/analyzer-decls.h"
#include <string>

void
test_ctor_small ()
{
  std::string s {"abc"};
  __analyzer_eval (s.length () == 3); // { dg-warning "TRUE" }
  __analyzer_eval (s.size () == 3); // { dg-warning "TRUE" }
  __analyzer_eval (__builtin_strlen (s.c_str ()) == 3); // { dg-warning "TRUE" }
}
