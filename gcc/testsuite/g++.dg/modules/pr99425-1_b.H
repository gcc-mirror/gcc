// { dg-additional-options {-fmodule-header -fdump-lang-module-alias} }
// { dg-module-cmi {} }

#include "pr99425-1.h"

import "pr99425-1_a.H";

struct Cont
{
  int call ();
};

inline void widget (Cont parm)
{
  ssize (parm);
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s alias spec merge key \(new\) type_decl:'::make_signed_t'\n  ...  Read:-[0-9]*'s type spec merge key \(new\) type_decl:'::make_signed'\n  Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template ssize'} module } }

