/* Test that in pre-C23 modes, nullptr is a normal identifier,
   not a keyword.  */
/* { dg-options "-std=c17 -pedantic-errors" } */

int nullptr;

void
f (int nullptr)
{
}
