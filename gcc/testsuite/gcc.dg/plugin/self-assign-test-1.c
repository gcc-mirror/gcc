/* Test the self-assignemnt detection plugin.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct Bar {
  int b_;
  int c_;
};

int g;

int main()
{
  struct Bar *bar;
  int x = x; /* { dg-warning "assigned to itself" } */
  static int y;
  struct Bar b_array[5];

  b_array[x+g].b_ = b_array[x+g].b_; /* { dg-warning "self-assignment detected" "" { xfail *-*-* } } */
  g = g; /* { dg-warning "assigned to itself" } */
  y = y; /* { dg-warning "assigned to itself" } */
  bar->b_ = bar->b_; /* { dg-warning "assigned to itself" } */
}
