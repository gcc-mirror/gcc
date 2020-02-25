// { dg-additional-options "-fmodules-ts -fconcepts -fdump-lang-module-alias" }

#include "concept-5.h"
import "concept-5_a.H";

static_assert (f1 ('a') == 1);
static_assert (f1 (0xa) == 0);

// { dg-final { scan-lang-dump-times {named merge key \(matched\) template_decl:'::template f1'} 2 module } }
// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
