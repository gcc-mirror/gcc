/* PR middle-end/67512 */
/* { dg-do compile }  */
/* { dg-options "-O -Wuninitialized" } */

extern int fn2 (void);
extern int fn3 (int);
void
fn1 (void)
{
  int z, m;
  if (1 & m) /* { dg-warning "is used uninitialized" } */
    z = fn2 ();
  z = 1 == m ? z : 2 == m;
  fn3 (z);
}
