// PR c++/29911 (9381)
// { dg-options -std=c++0x }
// { dg-do run { target i?86-*-* x86_64-*-* } }

extern "C" int printf(const char *, ...);

void *save_this;
int *save_addr1, *save_addr2;

int fail;

struct Base
{
  __attribute((regparm(3))) void
  set(int *addr1, int *addr2)
  {
    if (this != save_this)
      {
	++fail;
	printf("error! this == %p, should be %p\n", this, save_this);
      }
    if (addr1 != save_addr1)
      {
	++fail;
	printf("error! addr1 == %p, should be %p\n", addr1, save_addr1);
      }
    if (addr2 != save_addr2)
      {
	++fail;
	printf("error! addr2 == %p, should be %p\n", addr2, save_addr1);
      }
  }
};

int main()
{
  void (__attribute((regparm(3))) Base::* pfm)(int *, int *) = &Base::set;
  __typeof (&Base::set) pfm2 = &Base::set;
  decltype (&Base::set) pfm3 = &Base::set;
  auto pfm4 = &Base::set;

  Base obj; save_this = &obj;
  int x, y; save_addr1 = &x; save_addr2 = &y;

  (obj.* pfm) (&x, &y);
  (obj.* pfm2) (&x, &y);
  (obj.* pfm3) (&x, &y);
  (obj.* pfm4) (&x, &y);

  return fail;
}
