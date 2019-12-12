/* { dg-do run } */

struct foo
{
  __UINT32_TYPE__ x;
};
typedef struct foo foo;

static inline __INT32_TYPE__ zot(foo *f)
{
  int ret;

  if (f->x > 0x7FFFFFFF)
    ret = (__INT32_TYPE__)(f->x - 0x7FFFFFFF);
  else
    ret = (__INT32_TYPE__)f->x - 0x7FFFFFFF;
  return ret;
}

void __attribute__((noinline,noclone)) bar(foo *f)
{
  __INT32_TYPE__ ret = zot(f);
  volatile __INT32_TYPE__ x = ret;
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
