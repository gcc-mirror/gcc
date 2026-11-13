/* Verify that the analyzer is silent on correct usage of std::string,
   using <string>.  */

// { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } }

/* Leaving assertions at default: we seem to need assertions at -O0
   for the test to pass.  */

// { dg-additional-options "--param=analyzer-supernode-explosion-factor=10" }

#include "../../../gcc.dg/analyzer/analyzer-decls.h"
#include <string>

void
test_ctor_large ()
{
  std::string s {"0123456789012345678901234567890123456789"
		 "0123456789012345678901234567890123456789"};
  __analyzer_eval (s.length () == 80); // { dg-warning "TRUE" }
  __analyzer_eval (s.size () == 80); // { dg-warning "TRUE" }
  __analyzer_eval (__builtin_strlen (s.c_str ()) == 80); // { dg-warning "TRUE" }
}
