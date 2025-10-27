//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cassert in pr95615.inc" { ! hostedlib } }

#define INITIAL_AWAIT_RESUME_THROWS 1
// This should reach unhandled_exception
#define SHOULD_CALL_UNHANDLED_EXCEPTION 1
#include "pr95615.inc"
