/* Test that 'class', 'public', 'private', protected', 'try', 'catch',
   'throw' are not keywords in pure Objective-C if not after a '@'.
*/
/* { dg-do compile } */

int class (int public)
{
  int private = public;
  int protected = private * 2;
  int try = protected * 2;
  int catch = try * 2;
  int throw = catch * 2;

  return throw;
}

int main (void)
{
  return class (0);
}
