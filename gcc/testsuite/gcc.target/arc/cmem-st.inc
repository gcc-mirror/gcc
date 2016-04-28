
struct foo_type
{
  unsigned int a;
  unsigned short b;
  unsigned char c;
};

struct foo_type foo CMEM_SECTION_ATTR

int
f ()
{
  foo.a = 3;
  foo.b = 2;
  foo.c = 1;
  return 0;
}
