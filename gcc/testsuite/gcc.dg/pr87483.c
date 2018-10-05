/* PR c/87483 */
/* { dg-require-alias "" } */

int f (void) { return 0; }
__attribute__ ((alias ("f"))) int g () { return 1; } /* { dg-warning "attribute ignored because function is defined" } */
__attribute__ ((alias ("f"))) int g2 ();

int h (void)
{
  return g2 ();
}

int main()
{
  return h();
}
