// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fdump-lang-module-graph" }
module;
#include <stdio.h>
#include <stdarg.h>
export module logger;
// { dg-module-cmi logger }

export void Log (char const *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  printf ("Logging:");
  vprintf (fmt, args);
  printf ("\n");
  va_end (args);
}

// { dg-final { scan-lang-dump {Dependencies of decl function_decl:'::Log'} module } }
// not reachable (body not inline)
// { dg-final { scan-lang-dump-not {Reachable GMF '::printf[^\n']*' added} module } }

