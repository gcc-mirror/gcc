//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cassert in pr95615.inc" { ! hostedlib } }

#define INITIAL_SUSPEND_THROWS 1
// It should be irrelevant that unhandled_exception might throw, so we should
// not call it.
#define UNHANDLED_EXCEPTION_THROWS 1
#include "pr95615.inc"
