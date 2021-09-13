// { dg-additional-options {-fmodules-ts -fdump-lang-module } }
export module hello;
// { dg-module-cmi hello }

import "pr99389_a.H";

export inline bool Check (const string_view& n)
{
  return !n.empty ();
}

// { dg-final { scan-lang-dump {Pending specialization '::basic_string_view<char>' entity:. section:. keyed to '::basic_string_view'} module } }
