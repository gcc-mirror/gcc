
struct foo_type
{
  unsigned int a;
  unsigned short b;
  unsigned char c;
};

struct foo_type foo __attribute__ ((section (".cmem")));

unsigned int
f ()
{
  unsigned int tmp = foo.a + foo.b + foo.c;
  return tmp;
}
