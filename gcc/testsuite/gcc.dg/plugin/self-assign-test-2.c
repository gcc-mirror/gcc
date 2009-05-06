/* Test the self-assignemnt detection plugin with the 'disable' argument.  */
/* { dg-do compile } */
/* { dg-options "-O -fplugin-arg-selfassign-disable" } */

struct Bar {
  int b_;
  int c_;
};

int g;

int main()
{
  struct Bar *bar;
  int x = x; /* { dg-bogus "assigned to itself" } */
  static int y;
  struct Bar b_array[5];

  b_array[x+g].b_ = b_array[x+g].b_; /* { dg-bogus "self-assignment detected" } */
  g = g; /* { dg-bogus "assigned to itself" } */
  y = y; /* { dg-bogus "assigned to itself" } */
  bar->b_ = bar->b_; /* { dg-bogus "assigned to itself" } */
}
