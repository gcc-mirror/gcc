/* PR c++/3406 */
/* { dg-do compile } */
int main()
{
  // The parentheses around the expression caused parse errors before 3.4.
  ( int() > int() );
  return 0;
}
 
