//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cassert in pr95615.inc" { ! hostedlib } }

#define INITIAL_AWAIT_READY_THROWS 1
#include "pr95615.inc"
