// { dg-do assemble  }
// Bug: C++ semantics for assignment don't match the backend semantics for
// MODIFY_EXPR.

void
foo (int j)
{
  (j = 1)++;			// causes compiler segfault
}
