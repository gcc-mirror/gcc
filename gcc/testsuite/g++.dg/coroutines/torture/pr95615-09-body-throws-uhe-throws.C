//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cassert in pr95615.inc" { ! hostedlib } }

#define BODY_THROWS 1
// This should reach unhandled_exception...
#define SHOULD_CALL_UNHANDLED_EXCEPTION 1
// ... which will throw 
#define UNHANDLED_EXCEPTION_THROWS 1
// and we should cleanup by calling destroy.
#include "pr95615.inc"
