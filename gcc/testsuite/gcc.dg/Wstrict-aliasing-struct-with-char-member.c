/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct a {
    int i;
    char c;
};
struct b {
    float f;
    float g;
};
int main(void)
{
  static struct b b;
  return ((struct a *)&b)->i; /* { dg-warning "will break strict-aliasing" } */
}
