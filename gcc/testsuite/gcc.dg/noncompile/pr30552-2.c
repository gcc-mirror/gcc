/* PR c/30552 */

/* Another example of a statement expression as formal array argument size in
 * nested old-style function declaration should generate user error, not 
 * internal compiler error.  */

/* { dg-do compile } */
/* { dg-options "" } */

int main()
{
  void fun(a)
    int a[({int b=2; b;})]; /* { dg-error "braced-group within expression allowed only inside a function" } */
  {
  }
  return 0;
}
