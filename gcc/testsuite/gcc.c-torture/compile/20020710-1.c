/* Red Hat bugzilla #68395
   PR middle-end/7245
   This testcase ICEd on IA-32 because shift & compare patterns
   predicates allowed any immediate, but constraints allowed only
   numbers from 1 to 31.  */

void foo (int *x, unsigned int y)
{
  int a = y >> -13;
  if (a)
    *x = a;
}
