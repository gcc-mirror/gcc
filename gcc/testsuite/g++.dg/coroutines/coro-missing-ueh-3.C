//  { dg-additional-options "-fsyntax-only -fno-exceptions -Wno-pedantic" }

/* We don't warn about the missing method, unless in pedantic mode, so
   this compile should be clean.  */

#include "coro.h"
#include "coro-missing-ueh.h"

MissingUEH
bar ()
{ 
  co_return;
}

// check we have not messed up continuation of the compilation.
template <class... Args>
struct void_t_imp {
  using type = void;
};

int main (int ac, char *av[]) {
  MissingUEH x = bar ();
  return 0;
}
