// PR c++/40445
// Check that a function containing only __builtin_unreachable()
// doesn't ICE.

// { dg-do compile }
// { dg-options "-O0" }
const char *
f (void)
{
  __builtin_unreachable ();
}
