/* PR c/30552 */

/* Statement expression as formal array argument size in nested old-style 
   function declaration should generate user error, not internal compiler 
   error.  */

/* { dg-do compile } */
/* { dg-options "" } */

int main()
{
  void fun(int a) /* { dg-error "old-style parameter declarations in prototyped function definition" } */
    int a[({void h(){}2;})]; /* { dg-error "braced-group within expression allowed only inside a function" } */
  {
  }
  return 0;
}
