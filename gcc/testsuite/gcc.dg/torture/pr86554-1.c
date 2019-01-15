/* { dg-do run } */

struct foo
{
  unsigned x;
};
typedef struct foo foo;

static inline int zot(foo *f)
{
  int ret;

  if (f->x > 0x7FFFFFFF)
    ret = (int)(f->x - 0x7FFFFFFF);
  else
    ret = (int)f->x - 0x7FFFFFFF;
  return ret;
}

void __attribute__((noinline,noclone)) bar(foo *f)
{
  int ret = zot(f);
  volatile int x = ret;
  if (ret < 1)
    __builtin_abort ();
}

int main()
{
  foo f;
  f.x = 0x800003f8;

  bar(&f);
  return 0;
}
