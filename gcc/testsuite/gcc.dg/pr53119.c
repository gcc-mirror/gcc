/* { dg-do compile } */
/* { dg-options "-Wmissing-braces -Wmissing-field-initializers" } */

struct a {
  int x, y, z;
};

struct b {
  struct a w, z;
};

int main (void)
{
  struct a az = { 0 };
  struct a anz = { 1 };   /* { dg-warning "missing initializer for" } */
  struct a aez = { 0, 0 };   /* { dg-warning "missing initializer for" } */

  struct b bz = { 0 };
  struct b bnz = { 0, 0, 0, 0, 0, 0 };  /* { dg-warning "missing braces" }  */

  return 0;
}
