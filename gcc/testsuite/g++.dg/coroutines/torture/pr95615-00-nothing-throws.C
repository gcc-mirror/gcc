//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cassert in pr95615.inc" { ! hostedlib } }

// Check that this completes correctly if nothing throws.
#include "pr95615.inc"
