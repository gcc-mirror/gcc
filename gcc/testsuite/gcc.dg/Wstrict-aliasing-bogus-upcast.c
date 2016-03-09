/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct a {
    int i;
};
struct b {
    struct a a;
    int j;
};
int main(void)
{
  static struct b b;
  struct a *ap=(struct a *)&b;
  return ((struct b *)&ap->i)->j; /* { dg-bogus "will break strict-aliasing" } */
}

