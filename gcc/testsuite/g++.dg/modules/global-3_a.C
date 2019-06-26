// Testcase disabled for maintainance.
// { dg-module-do run { target noarch-*-noos } }
// { dg-do assemble { target noarch-*-noos } }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph" }
module;
#include <iostream>
export module logger;
// { dg-module-bmi logger { xfail *-*-* } }

export void Log (char const *msg)
{
  std::cout << "Logging:" << msg << "\n";
}

// { dg-final { scan-lang-dump {Dependencies of decl function_decl:'::Log@logger:1'} module } }
// not reachable (body not inline)
// { dg-final { scan-lang-dump-not {Reachable GMF '::printf' added} module } }

