// Bug: C++ semantics for assignment don't match the backend semantics for
// MODIFY_EXPR.
// Build don't link:

void
foo (int j)
{
  (j = 1)++;			// causes compiler segfault
}
