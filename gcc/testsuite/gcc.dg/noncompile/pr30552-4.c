/* PR c/30552 */

/* Statement expression as formal array argument size in nested function
 * prototype scope is valid.  */

/* { dg-do compile } */
/* { dg-options "" } */

int main()
{
  void fun(int a[({void h(){}10;})])
  {
  }
  return 0;
}
