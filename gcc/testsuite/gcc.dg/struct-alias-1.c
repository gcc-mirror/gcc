/* { dg-do link } */
/* { dg-options "-O2" } */

struct S {
   int a[3];
   int x;
};

extern void link_error(void);
static int i;

int main()
{
  struct S s;

  s.x = 0;
  s.a[i] = 1;
  if (s.x != 0)
    link_error ();

  return 0;
}
