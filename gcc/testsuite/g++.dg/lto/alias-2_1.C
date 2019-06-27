#include <string.h>
struct a {int a;} a;
struct b {int b;} b;
extern struct b **bptr;
void
inline_me_late (int argc)
{
  if (argc == -1)
    *bptr = (struct b *)(size_t)1;
}
void
init()
{
  a.a=1;
  b.b=2;
}
