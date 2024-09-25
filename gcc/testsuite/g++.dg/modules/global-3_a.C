// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
module;
#include <iostream>
export module logger;
// { dg-module-cmi logger }

export void Log (char const *msg)
{
  std::cout << "Logging:" << msg << "\n";
}

// { dg-final { scan-lang-dump {Dependencies of decl function_decl:'::Log'} module } }
// not reachable (body not inline)
// { dg-final { scan-lang-dump-not {Reachable GMF '::printf[^'\n]*' added} module } }

