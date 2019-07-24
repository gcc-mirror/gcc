struct a
{
  int foo,bar;
};
struct b
{
  struct a a[10];
};

extern  struct b *bptr;
extern  int i;

void
inline_me_late (void)
{
  bptr->a[i].foo=1;
}

