/* { dg-do run } */
/* { dg-options "-O1" } */

struct with_hole {
  int x;
  long y;
};
struct without_hole {
  int x;
  int y;
};
union u {
  struct with_hole with_hole;
  struct without_hole without_hole;
};

void __attribute__((noinline))
test (union u *up, union u u)
{
  union u u2;
  volatile int f = 0;
  u2 = u;
  if (f)
    u2.with_hole = u.with_hole;
  *up = u2;
}

int main(void)
{
  union u u;
  union u u2;
  u2.without_hole.y = -1;
  test (&u, u2);
  if (u.without_hole.y != -1)
    __builtin_abort ();
  return 0;
}
